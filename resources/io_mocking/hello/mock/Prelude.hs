{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

module Prelude (

    -- * Standard types, classes and related functions

    -- ** Basic data types
    Bool(False, True),
    (&&), (||), not, otherwise,

    Maybe(Nothing, Just),
    maybe,

    Either(Left, Right),
    either,

    Ordering(LT, EQ, GT),
    Char, String,

    -- *** Tuples
    fst, snd, curry, uncurry,

    -- ** Basic type classes
    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>), (>=), max, min),
    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),

    -- *** Numeric types
    Int, Integer, Float, Double, Rational, Word,

    -- *** Numeric type classes
    Num((+), (-), (*), negate, abs, signum, fromInteger),
    Real(toRational),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    Fractional((/), recip, fromRational),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isNegativeZero, isIEEE, atan2),

    -- *** Numeric functions
    subtract, even, odd, gcd, lcm, (^), (^^), fromIntegral, realToFrac,

    -- ** Semigroup and Monoid
    Semigroup((<>)),
    Monoid(mempty, mappend, mconcat),

    -- ** Functor, Applicative, Monad
    Functor(fmap, (<$)), (<$>),
    Applicative(pure, (<*>), (*>), (<*)),
    Monad((>>=), (>>), return),
    MonadFail(fail),
    mapM_, sequence_, (=<<),

    -- ** Foldable and Traversable
    Foldable(foldMap, foldr, foldl, foldr1, foldl1, elem, maximum, minimum, sum, product),
    Traversable(traverse, sequenceA, mapM, sequence),

    -- ** Miscellaneous functions
    id, const, (.), flip, ($), until,
    asTypeOf, error, errorWithoutStackTrace, undefined,
    seq, ($!),

    -- * List operations
    map, (++), filter, head, last, tail, init, (!!), null, length, reverse,
    -- *** Special folds
    and, or, any, all,
    concat, concatMap,
    -- ** Building lists
    -- *** Scans
    scanl, scanl1, scanr, scanr1,
    -- *** Infinite lists
    iterate, repeat, replicate, cycle,
    -- ** Sublists
    take, drop, takeWhile, dropWhile, span, break, splitAt,
    -- ** Searching lists
    notElem, lookup,
    -- ** Zipping and unzipping lists
    zip, zip3, zipWith, zipWith3, unzip, unzip3,
    -- ** Functions on strings
    lines, words, unlines, unwords,

    -- * Converting to and from @String@
    -- ** Converting to @String@
    ShowS,
    Show(showsPrec, show, showList),
    shows, showChar, showString, showParen,
    -- ** Converting from @String@
    ReadS,
    Read(readsPrec, readList),
    reads, readParen, read, lex,

    IO,
    putChar,
    putStr, putStrLn, print,
    getChar, getLine, getContents, interact,
    FilePath,
    readFile, writeFile, appendFile, readIO, readLn,
    IOError,
    ioError, userError
  ) where

import "base" Prelude hiding (FilePath, IO, getLine, getChar, readIO, readLn, putStr, putStrLn, putChar, print,
                              readFile, writeFile, appendFile, getContents, interact, ioError, userError)
import Mock.System.IO.Internal
