# Changes in 1.1.1

- `Unbox` instance for `BinomAcc` is added.


# Changes in 1.1.0

- Type classes `CalcMean` and `CalcVar` are generalized to use `MonadThrow` to
  signal failure instead of using `Maybe` only

- Functions for computing standard deviation are placed into type
  classes. Sometimes we have standard deviation at hand, if distribution is
  parameterized by it for example.

- `Mean` now type synonym for `MeanKBN`.

- `WelfordMean` and `KahanMean` are moved to `D.M.S.Extra` module.

- Support for calculating weighted mean.

- `StatMonoid` instances for up to 4-tuples.

- `Max` now works correctly (#2).

- `PPair` for use in parallel computation is added.


# Changes in 1.0.0.0

- Type class definition changed: now it has both `addValue :: m → a → m` and
  `singletonMonoid :: a → m`

- `Mean` renamed as `WelfordMean`

- `Unbox` instances added for all data types.

- `BinomAcc` added.
