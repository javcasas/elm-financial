# elm-financial: Proper numeric datatypes for operating on currency data
If you are going to display and process financial data (money, prices, tax rates, quotes)
you need some kind of exact data type. If, instead, you use a float, the floating poing
rounding shenanigans will eventually bit you, adding a penny here or there, or
suddenly creating a number with many decimals out of thin air.

Try for example in your favourite JS repl:
>> 0.1 + 0.2
<- 0.30000000000000004

This is not a valid result, specially if you are adding 0.10 USD and 0.20 USD.

This library attempts to solve this problem in a financial environment, not a general one.

It offers a new data type: *Financial*
*Financial* supports the basic math operations with perfect accuracy: add, substract, multiplication and negate (opposite)
It also supports dividing, but it can't guarantee perfect accuracy when dividing, for obvious
mathematical reasons (1/3 = 0.33333333333...).

# Usage
Currently the library is in alpha stage, although it will reach beta soon.
The way to use it is:

1. Convert a number to a *Financial*

  * fromInt : Generates a *Financial* from an Int
  * withDecimalsFromInt : Generates a *Financial* from an Int specifying how many digits are decimals. For example, withDecimalsFromInt 2 102 would be used to generate a *Financial* for 1.02
  * genPercentage : Generates a *Financial* for calculating a percentage. genPercentage 12 generates a *Financial* 0.12. Then you can use it to extract 12% of another *Financial* by multiplying the two.

2. Combine *Financial*s

  * add, sub : Adds or substracts two *Financial*s. The resulting *Financial* has as many decimals as the *Financial* with the most decimals
  * opposite : Changes the sign of a *Financial*, turning positives into negatives and negatives into positives.
  * mul : Multiplies two *Financial*s. The resulting *Financial* has as many decimals as the sum of the decimals of the *Financial*s involved
  * div : Divides two *Financials*. The resulting *Financial* is created with as many decimals as possible

3. Post-process the resulting *Financial*

  * roundToDecimals : Rounds the *Financial* to have the specified decimals. If it has too few, it adds some. If it has too many, it removes decimals, rounding to the nearest digit.

4. Format the result as a String

  * toString : Converts the *Financial* to a String with standard math representation, as in 3.55 or -41.20.
  * toAccountingString : Converts the *Financial* to a String with that wonky accounting representation that uses parenthesis for negative, as in 3.55 or (41.20).

# Internal representation
Internally, the *Financial* is stored as two Ints: The value, and how many digits of the value are decimals. This way, we can add 0.1 + 0.2, by adding 1 + 2 while keeping in mind that it has 1 decimal digits. Thus 0.1 + 0.2 = (1 + 2) * 10^-1 = 3 * 10^-1 = 0.3.

You don't have to care that much about the internal representation. But you have to remember to round down when you have too many decimals.

# Precision issues
Because the internal representation is a couple of Ints, and the backend is Javascript, there is a limit on how many digits (and thus decimals) can your *Financial*s use. See max\_significative\_digits : Int to see how many digits you can use.
If you try to use more digits, the *Financial* will collapse into the *Invalid* state, which currently I consider better than showing and processing float numbers. This may change and improve in the future.

# Other info
* The module also exports a test suite, as a list of (String, Bool). If all the Bools are True, everything is supposed to be fine.
