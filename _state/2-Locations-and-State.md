---
layout: frontpage
title: Locations and Storage
use_math: true
categories: chapter ch2
---

# Locations and Storage

In its simplest form, storage is a place to put things so we can go back and get them.  Storage can come in many forms - tape, disk, memory, USB sticks, firmware - but it always allows for values to persist in some form over time.

Storage can be viewed abstractly as a collection of _locations_ that are values for storing other values.  Think of a location as a box and the value at a location as the contents of the box.  The location is a value in the sense that it is constant and does not change, but the contents of a location can change.  We _dereference_ a location when we want the value it stores.  Remember, a location is a value and as such can be calculated and returned without affecting the contained value.  Dereferencing gets the value out and returns it.

Storage exists in the background and is there to use whenever needed.  When we program we declare variables, pointers and references that all reference storage, but we don’t ever talk about storage as a whole unless we’re systems programmers.

The languages we’ve defined so far are _pure functional_ languages.  Functional in that every expression is a function returning a value and parameter passing is a the dominant means of communicating values among expressions.  Pure in that all identifiers are _immutable_.  Once created their values cannot be changed.  They can be shadowed, but never permanently changed.  There is literally no notion of a variable in the traditional sense motivating use of the term identifier when referring to symbols.

Languages with storage tend to be _imperative_ languages.  Imperative in that expressions are sequenced and storage is the dominant means of communicating values among expressions.  By their nature, imperative languages cannot be pure and storage is _mutable_.  The contents of a location can be changed leading to the concept of a _variable_.

To model storage we will add `new`, `deref` and `set` operators to FBAE.

$$\begin{align*}
t ::= & \; \mathsf{new}\; t  \\
  & \mid \mathsf{deref}\; t \\
  & \mid \mathsf{set}\; l\; t \\
\end{align*}$$

The `new` operation allocates and initializes a location.   Given some term, `t`, `new t` evaluates `t`, stores the result in a new storage location, and returns a reference to the location, called `l`.  `new` ensures the storage location is fresh and storing `t` ensures the storage location is initialized.  An common use of `new` is creating a storage location that can be referenced in a scope using `bind`.  The form:

```text
bind l = new 2+3 in ...
```

creates a new location and sets `l` to the new location and initializes the location with the result of evaluating `2+3`.  In the `bind` body `l` serves as a reference to the location.   Because `t` is evaluated before being stored, the storage contents are always values and the value stored at `l` is `5`.

The `deref` operation dereferences a location and returns the stored value.  Given some term, `t`, `deref t` evaluates `t` and returns the stored value associated with the location.  `t` must always be of type `TLoc`, however there is no guarantee what value type is stored in `t` making `deref` an interesting type inference problem.  We will revisit that issue later.  For we will focus on evaluation.  Given our previous `bind`, we can dereference `l` and get `5` back:

```text
bind l = new 2+3 in deref l
== 5
```

`deref` evaluates `l` to get a location and then returns the `5` used as the initial value in `new`.

The `set` operation changes the value in a storage location and is what makes storage interesting.  Given a location and a value, `set` replaces the stored value with the new value and returns the new value.  `set` changes storage contents and is our first example of a _computational effect_.  Given our previous `bind` again, we can `set` the valued stored at `l` to change its value:

```text
bind l = new 2+3 in
  set l ((deref l) + 1) ; (deref l)
 == 6
```

Once again `l` is initialized to reference a storage location initialized with `5`.  The `bind` body sets that location to contain `(deref l) + 1`.  As one would expect, `(deref l)` returns `5` `(deref l) + 1` evaluates to `6`.  The `set` operator replaces the contents of storage at `l` with `6`.   Now we see sequence at work.  The second `deref` operation gets the value just stored using `set`.  The the result of the overall `bind` operation is `6`.  Note that `set` also returns `6` and the second `deref` can be omitted with no change to the expression’s value.  It is there to emphasize that the value at `l` _changes globally_ as a result of the `set` operation.  `l` references `6`, not `5` in the second operation of the sequence.

Also noticed that the example is an implementation of `l:=l+l`.  Not literally, but it accomplishes what assignment does in a traditional _imperative_ language.

Some example usage of our new operations.

Create a new location and store `5` in it.  Then dereference the new location:

```text
deref (new l 5)
== 5
```
The result will be the stored value `5`.

Create a new location and store `5` in it.  Then set the new location to `6` and dereference:
```text
deref (set (new 5) 6)
== 6
```

The result is the new value stored in the new location.

Let's now use `bind` to store a location and perform some operations on it.  First, let's create a location and stored `5`, but this time store the new location in `loc`.  With `loc` named in the enviornment, we can do various things with it.

In this example we dereference `loc`, add `1`, and set the location to the result and return what's in the location:

```text
bind loc = new 5 in
  (set loc (deref loc) + 1) ; deref loc
```

This is a clever implementation of `loc := loc + 1`.  Because the store is emphemeral, it is updated in subsequent statements.  `deref loc` returns the new value.  It is also a clever example of why storage is different than an environment.  The environment the `set` term runs in is _the same_ environment the `deref` term runs in.  The store used by the `deref` term is the store _resulting from_ the `set` term.  The interpreter for this language must manage the store differently than the environment.

In the following example we create two identifiers that reference the same location, change one and see the impact on the other:

```text
bind m = new 5 in
  bind n = m in
    set m 6 ; deref n
== 6
```

The value bound to `m` by the outermost `bind` is a new location initialized to `5`.  The value bound to `n` is not a new location, but _the same_ location bound to `m`.  There is no magic here.  `m` is evaluated resulting in the location created by `new`.  That location is bound to `n` by the innermost `bind`.  The `set` operator changes the contents of the location bound to `m`.  It _does not_ change `m` or `n`.  However, when we dereference `n` we get the new value stored by `set`.  When two identifiers reference the same location in this we call it _aliasing_.  Aliasing is like having two pointers to the same structure in a traditional language.  Changing the referenced structure changes what both pointers point to.

In contrast, we can shadow `n` just as we always have by nesting bindings.  Here the inner `n` has new location referencing `5`that is separate from the location bound to the previous `n` or `m`.  The value referenced by the innermost `n` is not impacted by the use of `set`;

```text
bind m = new 5 in
  bind n = m in
    bind n = new 5 in
      set m 6 ; deref n
== 5
```

So far so good.  Now for something a bit more adventurous.  Let’s define a function that takes a location as a parameter and sets the location to a new value:

```text
bind inc = (lambda l in (set l ((deref l) + 1)))
  bind n = new 5
    inc n ; deref n
== 6
```

This `inc` is a bit different than earlier versions.  It accepts a location as its parameter, dereferences the location and adds `1` to the result, and sets the location to the incremented value.  The inner bind creates a location containing `5` and binds it to `n`.  When `inc` evaluated, `n` is evaluated to return the location bound to it.  It is a location and not the contents of the location.  Now `inc` is called and the location dereferenced then updated.  The nature of storage and locations implies that unlike an environment, storage is mutable across binds and functional calls.  Changes to location contents are permanent and not discarded when the function’s scope closes.

One more example before talking about implementation.  Locations can store any value.  There are currently no restrictions on what those values might be.  Let’s store something more interesting than a number and see what we might do:

```text
bind inc = new (lambda l in (set l ((deref l) + 1))) in
  bind f = (lambda l in (lambda n in (((deref l) n)))) in
    bind g = new 5 in
      ((f inc) g) ; deref g
== ??
```

`inc` remains unchanged from the previous definition except the addition of `new`.  There is no magic here.  `inc` is a location containing the same function as before.  Remember, `lambda`’s are values and can be treated as such.  Storing the `lambda` in a location is no big deal.  `f` is a nested lambda and should be thought of as a curry-stle function.  The first parameter must be a location because it is dereferenced.  The result is the first argument in the application and must be a function.  So, `l` most be a location containing a function.  The second argument to `f` is used as an argument to the dereference function.  It must also be a location.  Thankfully, `g` is just a location containing `5`.

The body of the bind applies `f` to `inc` and the resulting function to `g`.  `(deref l)` in `f`becomes our old `inc` function and is applied to `g`, the location containing `5`.  Because `inc` sets the location passed as `l` the contents of the location bound to `g` are also changed.  Finally, a dereference of `g` results in `6`.  All that work for a`6`.

Several take-always here.  One is that calling `f` on a different function would apply that different function.  This is the beginnings of object orientation where method selection involves choosing a function to be called.  Two is writing and debugging this stuff can be a nightmare.  After we implement the basic storage constructs we will talk about how to restrict what we do a bit to make things much easier to debug and write.  Three is this is the beginnings of how boot works.  The infrastructure that boots the computer you are using determines what code to run when.  In a real sense, that’s what `f` does - runs code that is given to it.

## Implementing Store

To implement mutable store we need to define a data type for representing locations and stores and the extend the interpreter include the three new operations plus sequence.

### Store As a Function

There are many ways to define a storage structure in Haskell.  A list of pairs, some kind of array-like structure, or a linked list all come to mind.  We will use a technique using a function to represent storage that is common in the formal methods and modeling community.  This technique is descriptive in that it will define how storage should behave and not necessarily how it should be implemented.

The new type `Sto` will implement a basic concept of storage.   `Sto` has the following definition:

```haskell
Sto :: Loc -> Maybe FBAEVal
```

`Sto` will be a mapping from location to either a value or nothing.  Seems like an excellent definition of storage.  Given `s::Sto` then `s l` for some `l::Loc` will return a value or will return nothing.

The simplest way to model a location is using integers.  This is necessarily flawed because locations are not negative and there are not an infinite number of them.  For our purposes, integers plus a bit of care will be fine:

```haskell
type Loc = Int
```

The simplest store to think about is an empty store.   The empty store contains no values and all locations should return `Nothing` if accessed.  A function with this behavior is easily defined  as:

```haskell
initSto :: Sto
initSto x = Nothing
```

The function `initSto` will return `Nothing` for every input location.  Exactly the behavior we wanted from our empty store.  Given any location `initSto l` returns `Nothing`.

Updating storage to contain new values is a matter of creating a new `Sto` function with the property that every location except the updated location will return their original value while the updated location returns the new value.  Lets assume `s::Sto` is an existing store and we would like to update `s 1` to contain `5`.  We need a new function that returns `s l` for every location other than `1` and `5` for location `1`.  Such a function is rather easy to define:

```haskell
\l -> if l==1 then Just 5 else m l
```

In this new store if `l` is `1` then `Just 5` is returned.  However, if `l` is not `1`, then whatever is associated with `l` in the original store, `m`, is returned.  This is precisely what we want.  We can write a simple utility function that takes a store, a location and a new value and produces a new store:

```haskell
setSto :: Sto -> Loc -> FBAEVal -> Sto
setSto s l v = \m -> if m==l then (Just v) else (s m)
```

Starting with the initial store defined above, repeated applications of `setSto` will associate values with locations.  Any location that is not explicitly set will return `Nothing` when dereferenced.  It should be clear that if a location is set multiple times, the latest value is always returned.

Unfortunately, `new` introduces a hiccup in our nice memory model.  Given a value, `new` allocates a fresh location and stores the value in that location.  The trick is accomplishing _fresh location_.  We need to ensure that every new location has not been used before.

The simplest way to implement `new` is to allocate locations starting with `0` and counting up with each call to `new`. The first location allocated is `0`, the section `1`, the third `2` and so forth.  We then must ensure that storage cannot be allocated without calling `new`. The latter is easy.  Locations do not have a concrete syntax and cannot be operated on other that dereference.  No pointer arithmetic or casting a number to a location.  Locations are values that cannot be changed and cannot be generated other than using `new`.

To implement the monotonic counter let’s define a new type, `Store` that is a `Loc`/`Sto` pair:

```haskell
type Store = (Loc,Sto)
```

The `Loc` element represents the next location to allocate and the `Sto` element is the store value mapping locations to `Maybe FBAEVal`.  We now now define all our operations over this new type.

First we define an initial store, `initStore`.  The initial store is initialized with `initSto` as the store value and `0` as the next fresh location.  No locations are allocated in the initial store and when we start allocating we will start with location 0:

```haskell
initStore :: Store
initStore = (0,initSto)
```

We could provide a new initial store that loads the store with values we want in memory.  Additionally, we can checkpoint execution by copying the store value during execution and restarting with that value.  For our purposes we will start out by assuming each program execution should start with empty, uninitialized memory.

`derefstore` returns the value stored at a particular location.  For `Store` we simply call `deref` on the `Sto` value and return the result:

```haskell
derefStore :: Store -> Loc -> Maybe FBAEVal
derefStore (i,s) l = deref s l
```

There is no need for range checking on the location value, `l` when calling `deref`.  Any unallocated location will return `Nothing` allowing an interpreter to manage bad location access.

`setStore` is the analog to `derefStore` and implemented in nearly the same way.  Here we want to return a new `Store` with the value at location `l` replaced by a new value:

```haskell
setStore :: Store -> Loc -> FBAEVal -> Store
setStore :: (i,s) l v = (i,(setSto s l v))
```

The next fresh location value does not change and the new store  is calculated using `setSto` to update the store value.  Like `derefStore` there is no need to check the location value. Remember there is no concrete syntax for locations.  The only way to generate a location is to call `new` and all results of `new` are set and in range.

Finally `newStore` that updates the store and allocates a fresh location.  Like `setStore`, the `setSto` function is used to update the store an location `i`, where `i` is the next fresh location.  Additionally, the next fresh location is updated by incrementing `i`:

```haskell
newStore :: Store -> Store
newStore (i,s) = ((i+1),(setSto s i v))
```

As we allocate more locations, `i` grows monotonically ensuring that new memory locations do not reuse already allocated locations.

## Store and Environment Passing interpreter

Before diving into using `Reader` and defining a new monad, `State`, for stateful computation we will build an interpreter using `Maybe` and explicitly passing environment and store.  The rationale for this is to see first-hand the difference in how environment and store are managed.  The datatypes `FBAE` and `TFBAE` represent the abstract syntaxe of terms and types respectively.  While we won't be writing a type checker yet, we will include types in the AST for easy integration of a type inference function later.

```haskell
data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Mult :: FBAE -> FBAE -> FBAE
  Div :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> TFBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  Boolean :: Bool -> FBAE
  And :: FBAE -> FBAE -> FBAE
  Or :: FBAE -> FBAE -> FBAE
  Leq :: FBAE -> FBAE -> FBAE
  IsZero :: FBAE -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  New :: FBAE -> FBAE
  Set :: FBAE -> FBAE -> FBAE
  Deref :: FBAE -> FBAE
  Seq :: FBAE -> FBAE -> FBAE
  deriving (Show,Eq)
```

```haskell
data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  TLoc :: TFBAE
  deriving (Show,Eq)
```

The values in our language with state are the same as before with the addition of a new location value.  Remember that locations result from evaluating `new` and are consumed by `set` and `deref`.  Thus it is necessary to include values for them in our interpreter.

```haskell
data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> TFBAE -> FBAE -> Env -> FBAEVal
  LocV :: Int -> FBAEVal
  deriving (Show,Eq)
```
The environment type remains the same - a list of string, value pairs that store bindings of values to identifiers in scope.  Nothing we have done with store has any impact on the environment in this interpreter.  Keep this in mind as we dive into the implementation.

```haskell
type Env = [(String,FBAEVal)]
```

With AST, values and environment defined we can now define `evalM` using a `Maybe` monad in exactly the way we have before.  Let's try reusing our previous definition for `evalM` with the addition of a store:

```haskell
evalM :: Env -> Store -> Just FBAEVal
```

Our new interpreter will take an environment, a store, and produce a value.  Let's start the interpreter by defining constant evaluation cases:

```haskell
evalM env sto (Num x) = return (NumV x)
evalM env sto (Lambda i t b) = return (ClosureV i t b env)
evalM env sto (Boolean b) = return (BooleanV b)
```

Both `Num` and `Boolean` represent values that are not interpretted further.  Thus the are interpretted to be `NumV` and `BooleanV` as before.  Nothing about `env` or `sto` has any impact on their values.

Next we have binary numerical operations for `+`,`-`,`*`, and `/`.  Each of these operations is structured identically, so it makes sense to present them as one collection as we have in the past.  Let's take the same approach as we've taken for previous interpreters looking only at `+` for the time being:

```haskell
evalM env sto (Plus l r) = do { (NumV l') <- (evalM env sto l) ;
                                (NumV r') <- (evalM env sto r) ;
                                return (NumV (l'+r')) }
```
As we have done in the past, each argument to `Plus` is evaluated with `env` and `sto`.  The values are summed and the result returne as a `NumV`.  Our friend `Maybe` makes certain the subterms evaluate to numbers or returns `Nothing` in the backgroun.  Perfect!

Let's look again an earlier example:

```text
bind loc = new 5 in
  (set loc (deref loc) + 1) ; deref loc
```

We haven't yet defined our store related operations, but we do know what they are supposed to do.  In this case, a new location is initialized with `5` and bound to `loc`.  `sto` is updated by `new` as defined earlier and `loc` holds the new location value.  Now we go to the `set` operation.  Now set `loc` to what is currently in `loc` plus `1`.  Looking at our proposed implementation we see `sto` passed as an argument to `evalM` so it will be available when we evaluate `deref` and `+`.  No problem, right?

Big problem.

Assuming `deref` works fine, what happens when `set` is performed?  `deref` gets the stored value, `+` adds `1` and `set` stores the new value in `sto`.  The second `deref` needs to see this new `sto` value, not the original `sto` value.  `env` is local to the scope of an operator and can be dropped when that scope closes.  This is what `Reader` did for us and what our earlier interpreters did manually.  `sto` is updated and those updates are seen by all subsequent operations.  Think of it as memory in a C program.  When you update memory referenced by a pointer, that update is seen by the remaining program.  That's clearly not what our interpreter does.  It makes changes locally and then drops them before moving on.  Somehow we need to keep track of changes to storage.

The manual way to do this is to return storage from all operations.  This seems quite odd and we would never pass storage around in an interpreter, but we can model storage behavior using this technique.  Let's go back and try it.  First, we'll change the definition of `evalM`:

```haskell
evalM :: Env -> Sto -> FBAE -> Maybe (Sto,FBAEVal)
```

This update causes `evalM` to return a pair containing the result value as before and a store value representing the state of storage after `evalM` is performed.  Starting our definition again with constant values we don't see much, if any impact:

```haskell
evalM env sto (Num x) = return (sto,(NumV x))
evalM env sto (Lambda i t b) = return (sto,(ClosureV i t b env))
evalM env sto (Boolean b) = return (sto,(BooleanV b))
```
Evaluating a constant has no impact on storage, so the result `sto` is the same as the input `sto`.

Things are much more interesting when we look at binary operations.  Let's look again at `Plus`:

```haskell
evalM env sto (Plus l r) = do { (sto',(NumV l')) <- (evalM env sto l) ;
                                (sto'',(NumV r')) <- (evalM env sto' r) ;
                                return (sto'',(NumV (l'+r'))) }
```

Instead of using `sto` repeatedly, `evalM` keeps track of `sto` after each operation and passes it to the next operation.  `sto'` is the store resulting from evaluating the left operand.  It is passed as the store to evaluation of the right operand.  The store resulting from evaluating the right operand is `sto''` and is returned by the term evaluation.  Instead of dropping changes to `sto` after each operand execution, the changes are propagated to the next.  Any changes resulting from evaluating operands are propagated to any operations occuring after the sum.  For completeness, here is the code for all binary numeric operations:

```haskell
evalM env sto (Plus l r) = do { (sto',(NumV l')) <- (evalM env sto l) ;
                                (sto'',(NumV r')) <- (evalM env sto' r) ;
                                return (sto'',(NumV (l'+r'))) }
evalM env sto (Minus l r) = do { (sto',(NumV l')) <- (evalM env sto l) ;
                                 (sto'',(NumV r')) <- (evalM env sto' r) ;
                                 return (sto'',(NumV (l'-r'))) }
evalM env sto (Mult l r) = do { (sto',(NumV l')) <- (evalM env sto l) ;
                                (sto'',(NumV r')) <- (evalM env sto' r) ;
                                return (sto'',(NumV (l'*r'))) }
evalM env sto (Div l r) = do { (sto',(NumV l')) <- (evalM env sto l) ;
                               (sto'',(NumV r')) <- (evalM env sto' r) ;
                               return (sto'',(NumV (div l' r'))) }
```
Mutable store changes the world. (Pun intended.)  Now we must think carefully about things like operation ordering that did not arise in a pure functional langauge without mutable storage.

Managing store in `Bind` and `App` is handled in roughly the same way as binary operations:

```haskell
evalM env sto (Bind i v b) = do { (sto',v') <- (evalM env sto v) ;
                                 evalM ((i,v'):env) sto' b }
evalM env sto (App f a) = do { (sto',(ClosureV i t b e)) <- (evalM env sto f) ;
                              (sto'',a') <- (evalM env sto' a) ;
                              (evalM ((i,a'):e) sto'' b) }
```

In `Bind` evaluating the bound value may change the store.  Thus the result store becomes the store for the body.  `App` is exactly a binary operation and is handled as such.  Evaluating the function argument results in `sto'` used when evaluating the parameter.  The resulting value `sto''` is used when evaluating the function body.  Note also that handling the environment and managing identifiers does not change in any way.  The addition of storage does not impact identifier management.

As one might suspect, the Boolean operators are handled like the mathematical operations.  Thus we will skip them except for `If`.  As one would expect, evaluating the conditional argument results in a value and a new store, `sto'`.  However, `sto'` is used in both `If` arms:

```haskell
evalM env sto (If c t e) = do { (sto',(BooleanV c')) <- (evalM env sto c) ;
                               (if c'
                                then (evalM env sto' t)
                                else (evalM env sto' e)) }
```

Only one arm is evaluated, thus the result of that evaluation is the only result to account for when calculating the final result.  So, `sto'` is passed to `evalM` in both cases.

`If` is interesting for another more subtle reason.  In a lazy language like Haskell that does not evaluate function arguments until they are used, `if` is just a function.  No special sauce needed.  In a strict language like Racket, `if` is a special form.  Strict languages evaluate _all_ parameters before evaluating a function.  Both the `then` and `else` arms are evaluated if we treat `if` as a function.  If that strict language has mutable memory, then evaluating both arguments could change the store even though only one result is used.  Until now we could have written our `if` interpreter to evaluate both `then` and `else` arms then choose the result based on the condition.  With mutable store, this is no more.

Storage manipulation operators, `New`, `Set`, and `Deref` are implemented using utility functions defined earlier.  Once again any store modifications resulting from operand evaluation must be passed along to subsequent evaluation:

```haskell
evalM env sto (New t) = do { ((i,m),v) <- (evalM env sto t) ;
                             return ((newLoc (i,m) v),(LocV i)) }
evalM env sto (Set l v) = do { (sto',(LocV l')) <- (evalM env sto l) ;
                               (sto'',v') <- (evalM env sto' v) ;
                               return ((setLoc l' sto'' v'),v') }
evalM env sto (Deref l) = do { (sto',(LocV l')) <- (evalM env sto l) ;
                               (case (openLoc l' sto') of
                                         Just v -> return (sto',v)
                                         Nothing -> Nothing) } ;
```

The oddball is `Deref` where `openLoc` uses a `Maybe` that must be coverted into the return type of `evalM`.  There are more elegant ways of doing this.  All that is happening is the `Maybe FBAEVal` that results from `openLoc` is convered to a `Mabye (Sto,FBAEVal)` that is returned by `evalM`.

`Seq` is in many ways the most straightforward of the new operations.  The left operand of `Seq` is evaluated first and the resulting store used as input to the right operand evaluation:

```haskell
evalM env sto (Seq l r) = do { (sto',_) <- (evalM env sto l) ;
                               (evalM env sto' r) }
```

The only thing worth noting is the return value resulting from evaluating the left operand is thrown away.  The `_` wildcard matches any value and cannot be used.  Thus, the only way sequenced operations can interact is via the store.  This is an interesting result.  The only way pure functions can interact is through parameter passing.  The only way impure statements can interact is through the store.  For this reason, sequence is not useful until we have a store.

## Discussion

As noted earlier, adding mutable state changes the world.  Our interprter becomes significantly more complex due to the miriad of ways effectful computing manifests itself.  However, the world is a stateful place.  Some operations - IO is the best example - simply cannot be easily modeled without introducing store and mutable state.  Ironically, the monad was introduced to handle defining just these sorts of things in functional languages.

We abandoned two things that will reappear in later chapters.  First, we have done no typechecking.  That is coming soon, but is again complicated by the shared, mutable properties of storage.  Second is monadic modeling.  We used the `Reader` monad to specifically represent the local storage computational feature.  Soon we will introduce the `State` monad to represent the mutable store computational feature.  The fun really starts when we combine all these things together into a glorious integrated interpreter!  That will be awhile, but it is coming.

## Definitions

- Imperative language - An imperative language uses sequencing as the dominant control structure and uses variables to communicate values.  Imperative languages have a mutable store.
- Functional language -  A functional language uses function calls and parameters as the dominant control structure and mechanism for communicating values.  Many functional languages have immutable store.
- Store - A collection of locations
- Location - A value that contains other values.  A location’s contents are mutable in that they can be changed.
- Dereferencing - Retrieving a value from a location
- Computational Effect - Changing global state in a persistent way.  Printing and setting variable values are effects.  Side effects are often considered bad programming practice, but an effect is more general.
- Aliasing - When two or more identifiers are bound to the same location.

## Exercises
