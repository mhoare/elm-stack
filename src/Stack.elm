module Stack exposing (initialise, push, pop, toList, top, Stack)

{-| This library implements a stack data structure in Elm, allowing you to worry more about your business logic and less about implementing common adts.

# Definition
@docs Stack

# Initialisation
@docs initialise

# Common Helpers
@docs pop, push, toList, top

-}


{-| -}
type Stack a
    = Stack (List a)


{-| Initialise an empty stack.
-}
initialise : Stack a
initialise =
    Stack []


{-| Convert a Stack type to a list data type
-}
toList : Stack a -> List a
toList (Stack stack) =
    stack


{-| Pushes an item onto the stack and returns the new stack. The item must be of the same type as the stack.
-}
push : a -> Stack a -> Stack a
push item (Stack stack) =
    Stack (item :: stack)


{-| Returns the top element of the stack without removing it.
-}
top : Stack a -> Maybe a
top (Stack stack) =
    List.head stack


{-| Removes the item at the top of the stack and returns it as the first item of a tuple.
-}
pop : Stack a -> ( Maybe a, Stack a )
pop (Stack stack) =
    case stack of
        [] ->
            ( Nothing, Stack [] )

        head :: tail ->
            ( Just head, Stack tail )
