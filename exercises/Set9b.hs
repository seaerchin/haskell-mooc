module Set9b where

import Data.Foldable (toList)
import Data.List
import qualified Data.Sequence as S
import Mooc.Todo

--------------------------------------------------------------------------------
-- Ex 1: In this exercise set, we'll solve the N Queens problem step by step.
-- N Queens is a generalisation of the Eight Queens problem described in
-- Wikipedia: https://en.wikipedia.org/wiki/Eight_queens_puzzle
--
-- We'll be working with a two-dimensional coordinate system for indexing the
-- queens on a chessboard of arbitrary size. (1,1) represents the top left
-- corner. (1,2) is the next square on the top row, (1,3) is the one after that,
-- (2,1) is the first square on the second row, (2,2) is the second square in
-- the second row, and so on. In general, the coordinates are of the form
-- (row,column). The idea is that the following arrangement of queens on a 8x8
-- board will be encoded as the list [(1,8),(2,6),(3,4),(5,7)] of coordinates:
--
--   .......Q
--   .....Q..
--   ...Q....
--   ........
--   ......Q.
--   ........
--   ........
--   ........
--
-- The first exercise is warmup. We'll define two helper functions that we're
-- going to use later: nextRow, and nextCol. nextRow increases the row by one
-- and sets column to 1. nextCol only increases the column by one. (By analogy,
-- nextRow works like line break and carriage return while nextCol works like
-- the space bar in a typewriter.)
--
-- Examples:
--   nextRow (1,1) ==> (2,1)
--   nextRow (4,7) ==> (5,1)
--   nextCol (1,1) ==> (1,2)
--   nextCol (4,7) ==> (4,8)
--
-- Before we start, remember type aliases? We define some of them just to make
-- the roles of different function arguments clearer without adding syntactical
-- overhead:

type Row = Int

type Col = Int

type Coord = (Row, Col)

nextRow :: Coord -> Coord
nextRow (i, j) = (i + 1, 1)

nextCol :: Coord -> Coord
nextCol (i, j) = (i, j + 1)

--------------------------------------------------------------------------------
-- Ex 2: Implement the function prettyPrint that, given the size of the
-- chessboard and a list of distinct coordinates of queens, prints the
-- chessboard with the queens on it. Empty squares must be printed as '.'s and
-- queens as 'Q's. The special line break character '\n' must be appended to the
-- end of each row.
--
-- Examples:
--   prettyPrint 3 [(1,1),(2,3),(3,2)] ==> "Q..\n..Q\n.Q.\n"
--   prettyPrint 3 [(2,3),(1,1),(3,2)] ==> "Q..\n..Q\n.Q.\n"
--   prettyPrint 3 [(1,3),(2,1),(3,2)] ==> "..Q\nQ..\n.Q.\n"
--
-- To see how the result looks like with the line breaks correctly printed, use
-- putStrLn in GHCI. To open this module in GHCI, run 'stack ghci Set9b.hs'.
--
-- Examples:
--   *Set9b> putStrLn $ prettyPrint 4 [(1,1),(1,4),(4,1),(4,4)]
--   Q..Q
--   ....
--   ....
--   Q..Q
--
--   *Set9b> putStrLn $ prettyPrint 4 [(4,4),(1,4),(1,1),(4,1)]
--   Q..Q
--   ....
--   ....
--   Q..Q
--
--   *Set9b> putStrLn $ prettyPrint 7 [(1,1),(2,3),(3,5),(4,7),(5,2),(6,4),(7,6)]
--   Q......
--   ..Q....
--   ....Q..
--   ......Q
--   .Q.....
--   ...Q...
--   .....Q.
--
-- Hint: Remember the function elem? elem x xs checks if the list xs contains
-- the element x, e.g. elem 1 [2,5,1] ==> True, elem 1 [2,5,2] ==> False.
--
-- Challenge: Try defining prettyPrint without elem by just iterating over all
-- coordinates one at a time. (For those who've had a course in data structures
-- and algorithms, this challenge is about finding an O(n^2) solution in terms
-- of the width (or height) n of the chess board; the naïve solution with elem
-- takes O(n^3) time. Just ignore the previous sentence, if you're not familiar
-- with the O-notation.)

-- just
prettyPrint :: Size -> [Coord] -> String
prettyPrint s cs =
  let board = replicate s (replicate s '.')
      cs' = map (\(x, y) -> (x - 1, y -1)) cs
   in intercalate "\n" (foldr update board cs') ++ "\n"

update :: Coord -> [[Char]] -> [[Char]]
update coord board = map toList (toList (update' coord b))
  where
    b = S.fromList (map S.fromList board)
    update' :: Coord -> S.Seq (S.Seq Char) -> S.Seq (S.Seq Char)
    update' (row, col) original = S.update row newRow original
      where
        boardRow = S.index original row
        newRow = S.update col 'Q' boardRow

--------------------------------------------------------------------------------
-- Ex 3: The task in this exercise is to define the relations sameRow, sameCol,
-- sameDiag, and sameAntidiag that check whether or not two coordinates of the
-- form (i,j) :: (Row, Col) on a table of indeterminate size are on the same
-- column, diagonal (top left to bottom right), or antidiagonal (bottom left to
-- top right) respectively. Indeterminate size of the table means that these
-- relations should work for tables of all sizes. (You may assume that all
-- coordinates will be positive.)
--
-- Examples:
--   sameRow (1,1) (1,1) ==> True
--   sameRow (1,1) (2,1) ==> False
--   sameRow (1,1) (1,2) ==> True
--   sameCol (1,1) (4,1) ==> True
--   sameCol (1,1) (4,2) ==> False
--   sameDiag (1,1) (2,2) ==> True
--   sameDiag (1,1) (1,2) ==> False
--   sameAntidiag (1,1) (1,2) ==> False
--   sameAntidiag (2,10) (5,7) ==> True
--   sameAntidiag (500,5) (5,500) ==> True

sameRow :: Coord -> Coord -> Bool
sameRow (i, j) (k, l) = i == k

sameCol :: Coord -> Coord -> Bool
sameCol (i, j) (k, l) = j == l

sameDiag :: Coord -> Coord -> Bool
sameDiag (i, j) (k, l) = i - j == k - l

sameAntidiag :: Coord -> Coord -> Bool
sameAntidiag (i, j) (k, l) = k - i == negate (l - j)

--------------------------------------------------------------------------------
-- Ex 4: In chess, a queen may capture another piece in the same row, column,
-- diagonal, or antidiagonal in one step. This danger zone, where pieces can be
-- captured by a queen (indicated here with the character '#') looks like this:
--
--   .#.#.#..
--   ..###...
--   ###Q####
--   ..###...
--   .#.#.#..
--   #..#..#.
--   ...#...#
--   ...#....
--
-- For multiple queens, the danger zone is the union of the danger zones for
-- individual queens. This means that all coordinates belonging to the danger
-- zones of one or more individual queens also belongs to the collective danger
-- zone of all queens on the board. For example, if we add a second queen to the
-- coordinates (4,6), the danger zone grows:
--
--   .###.#..
--   ..####.#
--   ###Q####
--   #####Q##
--   .#.####.
--   #..#.###
--   ..##.#.#
--   .#.#.#..
--
-- Implement the function danger that checks if a coordinate belongs to the
-- collective danger zone of the given list of (coordinates of) queens.
-- Graphically speaking, we want to check if the square at the given coordinates
-- looks like '.' rather than '#'. (You may assume that the given coordinate
-- will be different from all the coordinates in the stack.)
--
-- Examples:
--  danger (5,2) [] ==> False
--  danger (5,2) [(1,2)] ==> True
--  danger (5,2) [(4,3)] ==> True
--  danger (4,5) [(3,4),(4,6)] ==> True
--  danger (5,3) [(3,4),(4,6)] ==> False
--  danger (5,3) [(3,4),(4,6),(7,5),(6,2),(8,1)] ==> True
--
-- Hint: Use the relations of the previous exercise!
--
-- Lists of coordinates of queens will be later used in a First in Last Out
-- (LIFO) manner, so we give this type the alias Stack:
-- https://en.wikipedia.org/wiki/Stack_(abstract_data_type)

type Size = Int

type Candidate = Coord

type Stack = [Coord]

type Queen = Coord

type DangerZones = [Coord]

danger :: Candidate -> Stack -> Bool
danger c = any (checkDanger c)

checkDanger :: Candidate -> Queen -> Bool
checkDanger c q = or [c `sameCol` q, c `sameAntidiag` q, c `sameRow` q, c `sameDiag` q]

--------------------------------------------------------------------------------
-- Ex 5: In this exercise, the task is to write a modified version of
-- prettyPrint that marks those empty squares with '#' that are in the
-- collective danger zone of the given stack of queens. You may assume that
-- none of the queens in the stack are in the danger zone of another queen.
--
-- Examples:
--   *Set9b> putStrLn $ prettyPrint2 3 []
--   ...
--   ...
--   ...
--
--   *Set9b> putStrLn $ prettyPrint2 4 [(1,2),(2,4)]
--   #Q##
--   ###Q
--   .###
--   .#.#
--
--   *Set9b> putStrLn $ prettyPrint2 9 [(5,5)]
--   #...#...#
--   .#..#..#.
--   ..#.#.#..
--   ...###...
--   ####Q####
--   ...###...
--   ..#.#.#..
--   .#..#..#.
--   #...#...#
--
-- (For those that did the challenge in exercise 2, there's probably no O(n^2)
-- solution to this version. Any working solution is okay in this exercise.)

prettyPrint2 :: Size -> Stack -> String
prettyPrint2 s qs =
  let rows = [1 .. s]
      board = map (\row -> printer qs row s) rows
   in intercalate "\n" board ++ "\n"

-- Row denotes the row number
-- returns the current row
printer :: [Queen] -> Row -> Size -> String
printer qs r 0 = ""
printer qs r sz = printer qs r (sz - 1) ++ [curChar]
  where
    curChar
      | (r, sz) `elem` qs = 'Q'
      | (r, sz) `danger` qs = '#'
      | otherwise = '.'

--------------------------------------------------------------------------------
-- Ex 6: Now that we can check if a piece can be safely placed into a square in
-- the chessboard, it's time to write the first piece of the actual solution.
--
-- Given the size of the chessboard and a stack, the function fixFirst
-- should take the queen on the top of the stack, and if it is in in
-- danger, move it right _along the same row_ (in the direction of
-- increasing columns) until it is not in danger.
--
-- If no safe spot is found for the queen on that row, fixFirst should
-- return Nothing.
--
-- Examples:
--   fixFirst 5 [(1,1)] ==> Just [(1,1)]
--   fixFirst 5 [(3,4)] ==> Just [(3,4)]
--   fixFirst 5 [(1,1),(1,5)] ==> Nothing
--   fixFirst 5 [(1,1),(3,3)] ==> Just [(1,2),(3,3)]
--   fixFirst 5 [(1,3),(3,3)] ==> Just [(1,4),(3,3)]
--   fixFirst 5 [(2,1),(3,3)] ==> Just [(2,1),(3,3)]
--   fixFirst 8 [(8,1),(1,1)] ==> Just [(8,2),(1,1)]
--   fixFirst 8 [(4,1),(3,4),(4,6)] ==> Nothing
--   fixFirst 8 [(6,1),(3,4),(4,6)] ==> Just [(6,2),(3,4),(4,6)]
--   fixFirst 8 [(5,1),(3,8),(4,6),(7,5),(6,2),(8,1)] ==> Nothing
--
-- Hint: Remember prettyPrint and prettyPrint2? They might be useful
-- for debugging. For example we can run this to see what's happening
-- in that last example. The whole fifth row is in danger zone.
--
--   putStrLn $ prettyPrint2 8 [(3,8),(4,6),(7,5),(6,2),(8,1)]
--     ###.####
--     ##.#####
--     #######Q
--     #####Q##
--     ########
--     #Q######
--     ####Q###
--     Q#######

-- recur until top queen > size then return
fixFirst :: Size -> Stack -> Maybe Stack
fixFirst n (q: s) = moveQueenRight q s n 

-- move queen until no longer in danger
moveQueenRight :: Queen -> Stack -> Size-> Maybe Stack
moveQueenRight q@(x, y) s sz 
  | y > sz = Nothing 
  | q `danger` s = moveQueenRight (x, y + 1) s sz
  | otherwise = Just (q : s)

--------------------------------------------------------------------------------
-- Ex 7: We need two helper functions for stack management.
--

-- * continue moves on to a new row. It pushes a new candidate to the

--   top of the stack (front of the list). The new candidate should be
--   at the beginning of the next row with respect to the queen
--   previously on top of the stack.
--

-- * backtrack moves back to the previous row. It removes the top

--   element of the stack, and adjusts the new top element so that it
--   is in the next column.
--
-- Examples:
--   continue [(1,1)] ==> [(2,1),(1,1)]
--   continue [(2,3),(1,1)] ==> [(3,1),(2,3),(1,1)]
--   backtrack [(8,1),(7,5),(6,2),(4,6),(3,4)] ==> [(7,6),(6,2),(4,6),(3,4)]
--
-- Hint: Remember nextRow and nextCol? Use them!

continue :: Stack -> Stack
continue stk@(cur: qs) = 
    case cur of 
        (x, y) -> (x + 1, 1) : stk
    
backtrack :: Stack -> Stack
backtrack (_: xs) = 
    case xs of 
        ((a, b): ys) -> (a, b + 1): ys
--------------------------------------------------------------------------------
-- Ex 8: Let's take a step. Our algorithm solves the problem (in a
-- greedy manner) one row at a time, backtracking when needed. The
-- reason why we need backtracking is the following. We can greedily
-- put the queens to (1,1) and (2,3) and end up with no safe spot on
-- the third row:
--
--   Q###
--   ##Q#
--   ####
--   #.##
--
-- However if we backtrack and move the queen from (2,3) to (2,4), we
-- are able to place the third queen:
--
--   Q###
--   ###Q
--   #.##
--   ##.#
--
-- Implement the function step that takes the size of a board and a
-- stack, and tries to fix the position of the queen on the top of the
-- stack (using fixFirst). If a new position is found, the function
-- should call continue to return a stack with a new candidate. If a
-- safe position is not found, the function should call backtrack to
-- return a new stack.
--
-- Examples:
--
--   The first candidate is safe so we continue directly:
--     step 4 [(1,1)] ==> [(2,1),(1,1)]
--
--     Q...     Q...
--     .... ==> Q...
--     ....     ....
--     ....     ....
--
--   The second candidate needs to be adjusted a bit before a third is added:
--     step 4 [(2,1),(1,1)] ==> [(3,1),(2,3),(1,1)]
--
--     Q...     Q...
--     Q... ==> ..Q.
--     ....     Q...
--     ....     ....
--
--   No safe position is found for the third queen so we backtrack:
--     step 4 [(3,1),(2,3),(1,1)] ==> [(2,4),(1,1)]
--
--     Q...     Q...
--     ..Q. ==> ...Q
--     Q...     ....
--     ....     ....
--
--   The new position of the second queen is ok so we move to the third row:
--     step 4 [(2,4),(1,1)] ==> [(3,1),(2,4),(1,1)]
--
--     Q...     Q...
--     ...Q ==> ...Q
--     ....     Q...
--     ....     ....
--
--   More examples:
--     step 8 [(3,9),(4,6),(7,5),(6,2),(8,1)] ==> [(4,7),(7,5),(6,2),(8,1)]
--     step 8 [(4,7),(7,5),(6,2),(8,1)] ==> [(5,1),(4,7),(7,5),(6,2),(8,1)]

step :: Size -> Stack -> Stack
step sz stk = 
    let newStack = fixFirst sz stk 
    in case newStack of 
        Nothing -> backtrack stk 
        Just x -> continue x

--------------------------------------------------------------------------------
-- Ex 9: Let's solve our puzzle! The function finish takes a partial
-- solution (stack) and repeatedly step until a complete solution is
-- found.
--
-- Reminder: a complete solution has n queens that don't threaten each
-- other. One easy way to know you have a valid solution is when step
-- adds the (n+1)th queen.
--
-- After this, it's just a matter of calling `finish n [(1,1)]` to
-- solve the n queens problem.

finish :: Size -> Stack -> Stack
finish sz s
    | length s > sz = tail s 
    | otherwise = finish sz newStack
        where 
            newStack = step sz s 

solve :: Size -> Stack
solve n = finish n [(1, 1)]
