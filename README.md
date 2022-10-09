# purescript-default

 A type class which provides default value.

 ## Why this library?

Many times we need to pass some placeholder values as initial values to certain functions, e.g Passing empty string to Halogen Custom EdiText Component. 

```purescript
type PersonInfo = { name :: String, isMarried :: Boolean, salary :: Maybe Int }
```

To create empty placeholder value,

```purescript
defaultPersonInfo :: PersonInfo
defaultPersonInfo = defaultValue
```

It also comes in handy when you want to provide a default value, like in case of `fromMaybe`

```purescript
getPersonInfo :: Maybe PersonInfo -> PersonInfo
getPersonInfo mPersonInfo = fromMaybe defaultValue mPersonInfo
```

The DefaultValue type class is defined for handful of primitive types and also for some other types like Maybe, Either, Tuple etc.

But you can also use `defaultValue` for your custom made type by just creating a `DefaultValue` instance for that.

```purescript
data Color = RED | YELLOW | GREEN
instance defaultColorInstance :: DefaultValue Color where
  defaultValue = GREEN
```

```purescript
--- >>> defaultValue :: Color
--- >>> GREEN
```
