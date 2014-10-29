import Data.Char
import System.IO

house_size = 4000
grid_size = 100
xnum = house_size `div` grid_size
ynum = house_size `div` grid_size
theta_step = 5
tnum = 360 `div` theta_step
state_num = 72*40*40
xy_pair = [ (iy,ix,it) | iy <- [0..(ynum-1)] , ix <- [0..(xnum-1)] , it <- [0..(tnum-1)] ]
actions = ["fw","cw","ccw"]
one_step_cost = "100"
delta_fw = 10

main = do state_trans_header
          mapM_ stateTrans xy_pair
          hPutStr stderr $ unlines $ map (\x -> stateId x ++ " 0") $ filter isFinalState xy_pair

stateTrans :: (Int,Int,Int) -> IO ()
stateTrans s = mapM_ (stateTrans' s) actions

stateTrans' s a 
 | isFinalState s = return ()
 | otherwise = putStrLn header >> stateTrans'' s a
    where header = unwords ["state",stateId s,"action",a]

isFinalState :: (Int,Int,Int) -> Bool
isFinalState (iy,ix,it) = (ix == 13 ) && (iy == 5)

stateTrans'' (iy,ix,it) a
 | a == "fw" = stateTransFw (iy,ix,it)
 | otherwise = putStrLn $ '\t':out
    where delta = if a == "ccw" then 1 else -1
          it' = normalizeIT (it + delta)
          s' = stateId (iy,ix,it')
          out = unwords ["state",s',"prob. 1 cost", one_step_cost]

stateTransFw :: (Int,Int,Int) -> IO ()
stateTransFw s@(iy,ix,it) 
{--
 | theta == 0 = mapM_ (st s) $ stCheck 
                 [(0.1,(iy,ix+1,it)),(0.9,(iy,ix,it))]
--}
 | theta >= 0.0 && theta < 90.0 = mapM_ (st s) $ stCheck 
                 [(dxs/(gs*gs),(iy,ix+1,it)), (dys/(gs*gs),(iy+1,ix,it)),
                 (dxdys/(gs*gs),(iy+1,ix+1,it)), (xys/(gs*gs),(iy,ix,it))]
{--
 | theta == 90 = mapM_ (st s) $ stCheck 
                 [(0.1,(iy+1,ix,it)),(0.9,(iy,ix,it))]
--}
 | theta >= 90.0 && theta < 180.0 = mapM_ (st s) $ stCheck 
                 [(dxs/(gs*gs),(iy,ix-1,it)), (dys/(gs*gs),(iy+1,ix,it)),
                 (dxdys/(gs*gs),(iy+1,ix-1,it)), (xys/(gs*gs),(iy,ix,it))]
{--
 | theta == 180 = mapM_ (st s) $ stCheck 
                 [(0.1,(iy,ix-1,it)),(0.9,(iy,ix,it))]
--}
 | theta >= 180.0 && theta < 270.0 = mapM_ (st s) $ stCheck 
                 [(dxs/(gs*gs),(iy,ix-1,it)), (dys/(gs*gs),(iy-1,ix,it)),
                 (dxdys/(gs*gs),(iy-1,ix-1,it)), (xys/(gs*gs),(iy,ix,it))]
{--
 | theta == 270 = mapM_ (st s) $ stCheck 
                 [(0.1,(iy-1,ix,it)),(0.9,(iy,ix,it))]
--}
 | theta >= 270.0 && theta < 360.0 = mapM_ (st s) $ stCheck 
                 [(dxs/(gs*gs),(iy,ix+1,it)), (dys/(gs*gs),(iy-1,ix,it)),
                  (dxdys/(gs*gs),(iy-1,ix+1,it)), (xys/(gs*gs),(iy,ix,it))]
 | otherwise = error "error"
    where theta = 2.5 + fromIntegral (it * theta_step) 
          theta_rad = theta * 3.141592 / 180.0
          gs = fromIntegral grid_size
          dx = abs $ delta_fw * (cos theta_rad)
          dy = abs $ delta_fw * (sin theta_rad)
          dxs = dx * (gs - dy)
          dys = dy * (gs - dx)
          dxdys = dx * dy
          xys = gs * gs - (dxs + dys + dxdys)

st :: (Int,Int,Int) -> (Double,(Int,Int,Int)) -> IO ()
st s@(iy,ix,it) (p,(iy',ix',it')) = putStrLn ('\t':out)
    where s' = if isWall iy' ix' then stateId s else stateId (iy',ix',it')
          out = unwords ["state",s',"prob.",show p,"cost", one_step_cost]

{-- もし一つでも壁に当たったらemptyを返す --}
stCheck :: [(Double,(Int,Int,Int))] -> [(Double,(Int,Int,Int))]
stCheck es = if length cs == 0 then es else []
    where w (p,(y,x,t)) = isWall y x
          cs = filter w es

normalizeIT it = (it + tnum*100) `mod` tnum

isWall iy ix
 | ix < 0     || iy < 0     = True
 | ix >= xnum || iy >= ynum = True
 | ix `mod` 11 <= 6         = False
 | iy > 14 && iy < 25       = False
 | otherwise                = True

state_trans_header = mapM_ putStrLn ["%%metadata%%",
    "statenum " ++ (show state_num),
    "actions " ++ unwords actions,
    "",
    "%%state transitions%%" ] 

stateId :: (Int,Int,Int) -> String
stateId (y,x,t) = show (t + x*72 + y*72*40)
