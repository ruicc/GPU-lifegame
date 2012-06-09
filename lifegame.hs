{-# LANGUAGE FlexibleContexts #-}
module Main where
import Data.Array.Accelerate as Acc
import qualified Data.Array.Accelerate.CUDA as CUDA (run) 
import System.Random (getStdGen, randomRs)
import Control.Concurrent (threadDelay)
import Control.Applicative

type BoardSize = (Int, Int)
type Board = Array DIM2 Int

initLifeGame :: BoardSize -> IO Board
initLifeGame (x,y) = do
    gen <- getStdGen
    return $ Acc.fromList (Z:.x:.y) $ Prelude.take (x*y) $ randomRs (0,1) gen

step :: Board -> Acc Board
step arr = Acc.stencil stencil2D (Constant 0) . Acc.use $ arr
    where
        --stencil2D :: (Elt a, IsScalar a, Integral (Exp a)) => Stencil3x3 a -> Exp a
        stencil2D :: Stencil3x3 Int -> Exp Int
        stencil2D ((a,b,c), (d,e,f), (g,h,i)) = (cnt ==* 2) ? (e, (cnt ==* 3) ? (1, 0))
            where 
                cnt = a+b+c+d+f+g+h+i

repeatStep :: BoardSize -> Board -> IO ()
repeatStep siz board = do
    let board' = CUDA.run $ step board
    threadDelay 120000
    clearDisp
    printRect siz board'
    repeatStep siz board'
    where
        y = Prelude.snd siz
        printRect _ brd = printRect' $ toList brd
        printRect' brd 
            | length brd < y = return ()
            | otherwise = do
                mapM_ (putStr.show) $ Prelude.take y brd
                putChar '\n'
                printRect' $ Prelude.drop y brd
        clearDisp = putStr "\x1b[2J" >> putStr "\x1b[1;1H"

main :: IO ()
main = do
    x <- read <$> getLine
    y <- read <$> getLine
    let siz = (x,y)
    board <- initLifeGame siz
    repeatStep siz board
