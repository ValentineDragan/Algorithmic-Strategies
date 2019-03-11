-- Inf2d Assignment 1 2018-2019
-- Matriculation number: s1710228
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy) -- used in BestFirstSearch & aStarSearch for sorting the branches
import Data.Function (on) -- used in BestFirstSearch & aStarSearch for sorting the branches
import Debug.Trace
import TTTGame
import Data.Ix (inRange) -- used in the auxiliary funtion getAdjNodes to get values inRange(1,6)

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6



{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases. 
badNodesList = [(4,1), (4,2), (4,3), (4,4), (5,4)]
--badNodesList = [(3,1), (3,2), (3,3), (3,4), (2,4), (1,4)]
--badNodesList = []

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth = gridLength_search * gridWidth_search - length badNodesList
-- Why did you choose this number?
-- YOUR ANSWER GOES HERE
--
-- There are 36 maximum nodes in the grid (gridLength * gridWidth). Moreover, bad nodes can't be searched/explored.
-- Therefore, the maximum number of nodes that can be searched is gridLength * gridWidth - length(badNodesList).
-- If the maxDepth was higher than this number, some nodes would repeat in the path.
--

-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

-- The auxiliary function getAdjNodes is used to get the list of nodes adjacent to the head node.
-- The nodes are then appended to the branch if they aren't already contained in the branch, and are not bad nodes.
next::Branch -> [Branch]
next [] =  []
next branch = [nextNode:branch | nextNode <- adjNodes, not (elem nextNode branch), not (isBadNode(nextNode))]
    where adjNodes = getAdjNodes(head branch)



-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = (destination == curNode)


-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

-- The breadthFirstSearch function searches the branches in the following way:
-- If the search agenda is empty, we return Nothing (a path has not been found).
-- Otherwise, we check to see if we have arrived at the destination.
-- Otherwise, if the current node has already been explored then we skip this node.
-- Otherwise, we expand the search agenda by adding the next branches to the end of the agenda (BreadthFirst order)
breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch
breadthFirstSearch _ _ [] _ = Nothing
breadthFirstSearch destination next (currBranch:branches) exploredList
    | checkArrival destination currNode = Just currBranch
    | elem currNode exploredList = breadthFirstSearch destination next branches exploredList
    | otherwise = breadthFirstSearch destination next expandedBranches (currNode:exploredList)
        where currNode = head currBranch
              expandedBranches = branches ++ [x | x <- next currBranch, not (elem (head x) exploredList)]
    

             



-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.

-- The depthFirstSearch function is very similar to the BFS function above, except that:
-- we expand the search agenda by adding the next branches at the beginning of the agenda (DepthFirst order)
depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch
depthFirstSearch _ _ [] _ = Nothing
depthFirstSearch destination next (currBranch:branches) exploredList
    | checkArrival destination currNode = Just currBranch
    | elem currNode exploredList = depthFirstSearch destination next branches exploredList
    | otherwise = depthFirstSearch destination next expandedBranches (currNode:exploredList)
        where currNode = head currBranch
              expandedBranches = [x | x <- next currBranch, not (elem (head x) exploredList)] ++ branches

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.

-- The auxiliary function depth is used to calculate the depth of a branch (in this case, the number of nodes)
-- We check that each branch in the search agenda has a depth <= d
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> Maybe Branch
depthLimitedSearch _ _ [] _ = Nothing
depthLimitedSearch destination next (currBranch:branches) d
    | checkArrival destination currNode = Just currBranch
    | (depth currBranch == d) = depthLimitedSearch destination next branches d
    | otherwise = depthLimitedSearch destination next expandedBranches d
        where currNode = head currBranch
              expandedBranches = (next currBranch) ++ branches 


-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.

-- The iterDeepSearch function used the depthLimitedSearch function to check for a path, and
-- increments the depth until a solution is found.
iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next initialNode d
    | (d == maxDepth+1) = Nothing
    | (dlsResult == Nothing) = iterDeepSearch destination next initialNode (d+1)
    | otherwise = dlsResult
        where dlsResult = depthLimitedSearch destination next [[initialNode]] d

-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan::Node->Node->Int
manhattan position destination = abs (fst position - fst destination) + abs (snd position - snd destination)

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

-- The functions sortBy, compare and on, are used to sort the search agenda by the heuristic value (lowest first)
-- We apply the heuristic function to each head-node of a branch, and we sort branches by this value.
bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch _ _ _ [] _ = Nothing
bestFirstSearch destination next heuristic (currBranch:branches) exploredList
    | checkArrival destination currNode = Just currBranch
    | elem currNode exploredList = bestFirstSearch destination next heuristic branches exploredList
    | otherwise = bestFirstSearch destination next heuristic expandedBranches (currNode:exploredList)
    where currNode = head currBranch
          expandedBranches = sortBy (compare `on` (heuristic . head)) (next currBranch ++ branches)
    
    
-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

-- The auxiliary function aStarHeuristic is used to sum the value of heuristic and cost,
-- and then sort the search agenda by this value (lowest first).
aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch _ _ _ _ [] _ = Nothing
aStarSearch destination next heuristic cost (currBranch:branches) exploredList
    | checkArrival destination currNode = Just currBranch
    | elem currNode exploredList = aStarSearch destination next heuristic cost branches exploredList
    | otherwise = aStarSearch destination next heuristic cost expandedBranches (currNode:exploredList)
    where currNode = head currBranch
          expandedBranches = sortBy (compare `on` (aStarHeuristic heuristic cost)) (next currBranch ++ branches)
    
    
-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch  -> Int
cost branch = length branch - 1


-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches. 



-- | Section 5.1 Tic Tac Toe


-- | The eval function should be used to get the value of a terminal state. 
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
eval game
    | checkWin game 1 = 1
    | checkWin game 0 = -1
    | otherwise = 0

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state. 

-- The minimax function uses auxiliary functions maxvalue and minvalue to return the value of the state.
-- maxvalue returns the value when it's max-player's turn
-- minvalue returns the value when it's min-player's turn
minimax:: Game->Player->Int
minimax game player
    | terminal game = eval game
    | player == 1 = maxvalue game player
    | player == 0 = minvalue game player


-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state. 

-- We use auxiliary functions abMaxValue and abMinValue to return the value of the state using alpha beta pruning.
-- The two functions also use the auxiliary functions abMinList and abMaxList
-- to loop through the list of states that a player can move to, and prune the bad ones.
-- The values -2 and +2 are used to represent -/+ infinity for alpha-beta values
alphabeta:: Game->Player->Int
alphabeta game player
    | player == 1 = abMaxValue game player (-2) 2
    | player == 0 = abMinValue game player (-2) 2


-- | Section 5.2 Wild Tic Tac Toe





-- | The evalWild function should be used to get the value of a terminal state. 
-- It should return 1 if either of the move types is in the correct winning position. 
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game = abs(eval game)



-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward. 
-- If the min player sent the game into a terminal state you should give -1 reward. 

alphabetaWild:: Game->Player->Int
alphabetaWild game player = undefined
    
    
    
-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.

        
-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning). 
-- The evalWild function should be used to get the value of a terminal state. 

minimaxWild:: Game->Player->Int
minimaxWild game player = undefined
    

            
            -- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
 


-- The getAdjNodes function returns all adjacent nodes of (x,y) inside the 6x6 square.
getAdjNodes::Node -> [Node]
getAdjNodes (x, y) = [(x, y) | (x,y) <- adjacents, inRange(1,6) x, inRange (1,6) y]
    where adjacents = [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]

-- The isBadNode function checks if a node is contained in the list of bad nodes.
isBadNode::Node -> Bool
isBadNode (x, y) = elem (x, y) badNodesList

-- The depth function returns the depth of a current branch. This is needed for depthLimitedSearch function.
depth::Branch -> Int
depth branch = length branch

-- The aStarHeuristic function returns the value of a node in aStarSearch, by adding up the cost of the branch and the node heuristic.
aStarHeuristic::(Node->Int)->(Branch ->Int)->Branch->Int
aStarHeuristic heuristic cost branch = (heuristic node) + cost branch
    where node = head branch

-- The minvalue function returns the value of a current state when it's min-player's turn, using the minimax algorithm.
minvalue::Game->Player->Int
minvalue game player
    | terminal game = eval game
    | otherwise = minimum [maxvalue g (switch player) | g <- moves game player]

-- The maxvalue function returns the value of a current state when it's max-player's turn, using the mimimax algorithm.
maxvalue::Game->Player->Int
maxvalue game player
    | terminal game = eval game
    | otherwise = maximum [minvalue g (switch player) | g <- moves game player]

-- The abMaxValue function returns the value of a current state when it's max-player's turn, using alphabeta pruning
-- and uses the auxiliary function abMaxList to prune states.
abMaxValue::Game->Player->Int->Int->Int
abMaxValue game player alpha beta
    | terminal game = eval game
    | otherwise = foldl max (-2) (abMaxList (moves game player) (switch player) alpha beta)

-- The abMinValue function returns the value of a current state when it's min-player's turn, using alphabeta pruning
-- and uses the auxiliary function abMinList to prune states.
abMinValue::Game->Player->Int->Int->Int
abMinValue game player alpha beta
    | terminal game = eval game
    | otherwise = foldl min 2 (abMinList (moves game player) (switch player) alpha beta)

-- the abMaxList function loops through the list of states that max-player can move to,
-- and prunes the states where the beta-value is too low
abMaxList::[Game]->Player->Int->Int->[Int]
abMaxList [] player alpha beta = []
abMaxList (g:games) player alpha beta
    | currentValue >= beta = [currentValue]
    | otherwise = [currentValue] ++ abMaxList games player bestAlpha beta
        where currentValue = abMinValue g player alpha beta
              bestAlpha = max alpha currentValue

-- the abMinList function loops through the list of states that min-player can move to,
-- and prunes the states where the alpha-value is too high
abMinList::[Game]->Player->Int->Int->[Int]
abMinList [] player alpha beta = []
abMinList (g:games) player alpha beta
    | currentValue <= alpha = [currentValue]
    | otherwise = [currentValue] ++ abMinList games player alpha bestBeta
        where currentValue = abMaxValue g player alpha beta
              bestBeta = min beta currentValue