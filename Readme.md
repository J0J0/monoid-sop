We use [generics-sop][SOP] to implement generic `<>` and `mempty` functions
for product types whose factors are `Semigroup`s and `Monoid`s, respectively.

See the documentation within `Generics.SOP.Monoid` for an example.

*Note:* Since version `0.4.0.0` of `sop-core` and `generics-sop`, this is trivial. `:)`

[SOP]: https://hackage.haskell.org/package/generics-sop
