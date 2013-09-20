# Monadic Adventures

For the LA Haskell User Group

An introduction to monads and `mtl` by example.

## Monads Enable Effectful Composition

**XML Example**

Suppose

~~~{.haskell}
data XML

findChild :: String -> XML -> XML
~~~

Then we could write 

~~~{.haskell}
findChild "baz" . findChild "bar" . findChild "foo"
~~~

But in reality,

~~~{.haskell}
findChild :: String -> XML -> Maybe XML
~~~

We have to handle the *effect* of possible failure.

The function we need is not `(.)` but `<=<` from `Control.Monad`:

~~~{.haskell}
(.) :: (b -> c) -> (a -> b) -> a -> c
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
~~~

`<=<` handles the composition of effects, for some effect tracked by a monad `m`.

*Intuition 1*: Functions of type `a -> m b` are like functions of type `a -> b` but with side effects tracked by `m`.
*Intuition 2*: `return` embeds pure functions into a larger functional language supporting effects tracked by `m`.
*Intuition 3*: `<=<` is just normal function composition in this larger language.

## Monads Enable Do-Notation

`<=<` is defined in terms of the operator bind `>>=`, which together with the function `return` make up the `Monad` typeclass.

~~~{.haskell}
(>>=) :: Monad m => m a -> (a -> m b) -> m b

f <=< g a = g a >>= f
~~~

Any type constructor which is an instance of `Monad` can be used together with **do notation** to provide syntactic sugar for `>>=`:

~~~{.haskell}
fooBarBaz :: XML -> Maybe XML
fooBarBaz xml = findChild "foo" xml >>= \foo ->
                  findChild "bar" xml >>= \bar ->
                    findChild "baz" xml >>= \baz -> 
                      return baz
~~~

becomes

~~~{.haskell}
fooBarBaz :: XML -> Maybe XML
fooBarBaz xml = do foo <- findChild "foo" xml
                   bar <- findChild "bar" foo
                   baz <- findChild "baz" bar
                   return bar
~~~

But `>>=`, `>=>` and `return` are polymorphic - they work for any `Monad m`!

~~~{.haskell}
data File

fooBarBaz :: File -> IO File
fooBarBaz dir = do foo <- findChild "foo" dir
                   bar <- findChild "bar" foo
                   baz <- findChild "baz" bar
                   return bar
~~~

## Applicative is to Application what Monad is to Composition

A slight generalization of Monad: if we want to enable effectful function application, we can use `Applicative`

~~~{.haskell}
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
~~~

Compare this type signature with the type of `>>=`.

`<*>` does not provide access to the wrapped data like `>>=` does, so function arguments must be independent.

**XML Example**

~~~{.haskell}
data Person = Person { first :: String, last :: String, middleInitial :: String }

lookupPerson :: XML -> Maybe Person
lookupPerson xml = Person <$> findChild "first" xml <*> findChild "last" xml <*> findChild "middleInitial" xml
~~~

## Some Monads

From the mtl

- `List` - multiple return values
- `State` - mutable state
- `Reader` - global immutable state / configuration
- `Writer` - logging / accumulation
- `Maybe` - Possible failure
- `Either` - Possible failure with an error message

Some others

- `Identity` - no effects
- `IO` - interaction with the real world
- `STM` - later
- `ST` - pure state threads 
- `Free` - free models of algebraic theories

## An Example - State

~~~{.haskell}
newtype State s a = State { runState :: s -> (a, s) }
~~~

"A value of type `State s a` is a function which takes an initial state and returns a value of type `a` and a new state."

~~~{.haskell}
instance Functor (State s) where
  fmap f st = State $ \s -> let (a, s') = runState st s
                                       in (f a, s')
  
instance Monad (State s) where
  return a = State $ \s -> (a, s)
  fmap f st = State $ \s -> let (a, s') = runState st s 
                                       in runState (f a) s'
~~~

## Composing Monads

If `m` is a monad tracking events of a certain type, and `n` is another monad tracking events of another type, it seems reasonable that the composition `Both a = m (n a)` might track both types of effects, but `Both` is not an instance of `Monad` in general.

`mtl` is a library of monads which compose well with one another, made possible using typeclasses.

Monads which behave nicely under composition have instances of `MonadTrans` - they are *monad transformers*.

For example, compare `State` above, with its transformer equivalent `StateT`:

~~~{.haskell}
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
~~~

Effectful computations run in a monad which is made up of a *stack* of monad transformers.

- `StateT` - adds mutable state to the stack
- `WriterT` - adds logging / accumulation to the stack
- `ReaderT` - adds configuration data to the stack
- `ErrorT` - adds error handling to the stack

- `RWS` - adds all three of `ReaderT`, `WriterT` and `StateT` to the stack.

To run a computation, we need to peel off each effectful layer in the reverse order:

- `runStateT`, `execStateT`, `evalStateT`
- `runWriterT`,  
- `runReaderT`
- `runErrorT`

** Example **

~~~{.haskell}
data Log
data MyState
data Config

newtype MyStack a = MyStack { runMyStack :: StateT MyState (WriterT Log (ReaderT Config IO)) a } 
  deriving (Functor, Monad, MonadState MyState, MonadWriter Log, MonadReader Config, MonadIO)

main :: IO ()
main = do
  config <- loadConfigFromFile
  (_, log) <- runReaderT config
              . runWriterT
              . evalStateT initialState
              . runMyStack
              $ app
  print log

app :: IO ()
app = do tell $ Log "Starting..."
         config <- ask
         tell $ Log $ "Read config: " ++ show config
         anotherAction
         newState <- get
         liftIO $ print newState
~~~

## Lifting Actions

Suppose we have a monad transformer stack with `IO` on the very bottom.

We want to execute an action of type `IO a`, but we're working in the bigger stack `t1 (t2 ... (tn IO))) a`.

We could *lift* the action into the monad transformer stack:

~~~{.haskell}
lift :: (Monad m, MonadTrans t) => m a -> t m a
~~~ 

## Type Classes to the Rescue

When using monads from the `mtl`, we don't need to `lift` explictly. There are type classes provided which do the lifting for us.

- `MonadState`
- `MonadWriter`
- `MonadReader`
- `MonadError`

## New Monads From Old

Using newtype wrappers and `GeneralizedNewtypeDeriving` we can easily make new monads from these components.

We can hide constructors and only export those actions which we want users to have access to.

**Example**

~~~{.haskell}
module Random (runRandomT, next) where

newtype RandomT m a = RandomT { unRandomT :: StateT StdGen m a } deriving (Functor, Monad, MonadTrans)

runRandomT :: (Monad m) => RandomT m a -> StdGen -> m (StdGen, a)
runRandomT = runStateT . unRandomT
 
next :: (Random a) => (a, a) -> RandomT m a
next = undefined
~~~

## Three Monads

**Parsing**

~~~{.haskell}
newtype Parser input output = Parser { runParser :: S.StateT input Maybe output } 
  deriving (Functor, Applicative, Monad, S.MonadState input, MonadPlus, Alternative)
~~~

**Game**

Defined as a synonym for some type class constraints

~~~{.haskell}
type MonadGame item m = (Functor m, Monad m, S.MonadState (GameState item) m, W.MonadWriter (Reset [String]) m)
~~~

**Room State**

~~~{.haskell}
newtype R item m a = R { unR :: S.StateT ([item], String) m a } deriving (Functor, Monad)
~~~
