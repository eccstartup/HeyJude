module HeyJude
       (
         sing
       ) where

import Data.Char (toLower, toUpper)

sing :: String
sing = unlines $ [part 1, part 2, middle 1, part 3, middle 2, back] ++ replicate 3 nana

part n = unlines [tu hj ++ comma ++ "don't " ++ p1 n,
                  p2 n,
                  p3 n,
                  "Then you " ++ p4 n ++ " to make it better"]

p1 n = case n of 1 -> "make it bad"
                 2 -> "be afraid"
                 3 -> "let me down"

p2 n = case n of 1 -> "Take a sad song and make it better"
                 2 -> "You were made to go out and get her"
                 3 -> "You have found her, now go and get her"

p3 n
  | n == 1 = "Remember to let her into your heart"
  | n == 2 = "The minute you let her under your skin"
  | n == 3 = p3 1

p4 n
  | n == 1 = "can start"
  | n == 2 = "begin"
  | n == 3 = p4 1

middle n = unlines $ [m1 n ++ comma ++ hj ++ comma ++ m2 n,
                      m3 n,
                      m4 n,
                      m5 n,
                      m6 n]

m1 n = case n of 1 -> "And anytime you feel the pain"
                 2 -> "So let it out and let it in"

m2 n = case n of 1 -> "refrain"
                 2 -> "begin"

m3 n = case n of 1 -> "Don't carry the world upon your shoulders"
                 2 -> "You're waiting for someone to perform with"

m4 n = case n of 1 -> "For well you know that it's a fool who plays it cool"
                 2 -> "And don't you know that it's just you? " ++ tu hj ++ comma ++ "you'll do"

m5 n = case n of 1 -> "By making his world a little colder"
                 2 -> "The movement you need is on your shoulder"

m6 n = case n of 1 -> nnn ++ comma ++ na 2 ++ comma ++ na 4
                 2 -> m6 1 ++ comma ++ "yeah"

back = unlines $ [tu hj ++ comma ++ "don't " ++ p1 1,
                  p2 1,
                  "Remember to let her under your skin",
                  "Then you " ++ p4 2 ++ " to make it better",
                  "Better" ++ comma ++ concat (replicate 4 $ "better" ++ comma) ++ "oh!"]

nana = unlines $ concat $ replicate 2 [nnn ++ comma ++ lo nann, nann ++ comma ++ hj]
  where nann = "Na-na na na"

nnn = tu $ na 3

na n = unwords $ replicate n "na"

comma = ", "

hj = "hey Jude"

tu s = toUpper (head s) : tail s

lo = map toLower

{-
Hey Jude, don't make it bad
Take a sad song and make it better
Remember to let her into your heart
Then you can start to make it better

Hey Jude, don't be afraid
You were made to go out and get her
The minute you let her under your skin
Then you begin to make it better

And anytime you feel the pain, hey Jude, refrain
Don't carry the world upon your shoulders
For well you know that it's a fool who plays it cool
By making his world a little colder
Na na na, na na, na na na na

Hey Jude, don't let me down
You have found her, now go and get her
Remember to let her into your heart
Then you can start to make it better

So let it out and let it in, hey Jude, begin
You're waiting for someone to perform with
And don't you know that it's just you? Hey Jude, you'll do
The movement you need is on your shoulder
Na na na, na na, na na na na, yeah

Hey Jude, don't make it bad
Take a sad song and make it better
Remember to let her under your skin
Then you begin to make it better
Better, better, better, better, better, oh!

Na na na, na-na na na
Na-na na na, hey Jude
Na na na, na-na na na
Na-na na na, hey Jude

Na na na, na-na na na
Na-na na na, hey Jude
Na na na, na-na na na
Na-na na na, hey Jude

Na na na, na-na na na
Na-na na na, hey Jude
Na na na, na-na na na
Na-na na na, hey Jude
-}
