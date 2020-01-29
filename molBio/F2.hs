-- Filip Bäck
-- 2019-01-30
-- F2 - Molekylärbiologi i Haskell

module F2 where
import Data.List

-- Datatype
data MolSeq = MolSeq { sekvensnamn :: String
                       ,sekvens :: String
                       ,sekvenstyp :: SekvensTyp
                     } deriving (Eq, Show, Read)

data SekvensTyp = DNA | Protein deriving (Eq, Show, Read)

-- variables
nucleotides = "ACTG"
aminoacids = "ARNDCEQGHILKMFPSTWYV"

{- Takes name[of sequence] and the sequence itself and checks if it's dna or protein.
Uses 'checkifdna' as a helper to check for nukleotides in sequence. -}
checkifdna :: String -> Bool
checkifdna [] = True
checkifdna (x:xs)
    |x `elem` nucleotides = checkifdna xs
    |otherwise = False

string2seq :: String -> String -> MolSeq
string2seq sekvensnamn sekvens
    |checkifdna sekvens = MolSeq sekvensnamn sekvens DNA
    |otherwise = MolSeq sekvensnamn sekvens Protein

-- getName of sequence
seqName :: MolSeq -> String
seqName (MolSeq sekvensnamn _ _) = sekvensnamn

-- getSequence of sequence
seqSequence :: MolSeq -> String
seqSequence (MolSeq _ sekvens _) = sekvens

--getLength of sequence
seqLength :: MolSeq -> Int
seqLength (MolSeq _ sekvens _) = length sekvens

--getType of object
seqType :: MolSeq -> SekvensTyp
seqType (MolSeq _ _ sekvenstyp) = sekvenstyp

-- evolutionaryDistance of two profiles
-- a = number of positions where two sequences differ
seqDistance :: MolSeq -> MolSeq -> Double
seqDistance sekvens1 sekvens2
    |sekvenstyp sekvens1 /= sekvenstyp sekvens2 = error "Uncompatible sequences"
    |sekvenstyp sekvens1 == DNA = jukesCantor a
    |otherwise = poisson a
        where
            a = fromIntegral b / fromIntegral c
            b = hammingDistance (seqSequence sekvens1) (seqSequence sekvens2)
            c = seqLength(sekvens1)

-- Calculate 'a' for 'Jukes-Cantor' and 'Poisson' models.
hammingDistance :: String -> String -> Int
hammingDistance [] [] = 0
hammingDistance (x:xs) (y:ys)
    |x == y = hammingDistance xs ys
    |otherwise = 1 + hammingDistance xs ys

-- Jukes-Cantor - calculates distance between two DNA-sequences.
jukesCantor :: Double -> Double
jukesCantor a
    |a <= 0.74 = -3/4 * log(1- (4*a)/3)
    |otherwise = 3.3

-- Poisson - calculates distance between two protein-sequences.
poisson :: Double -> Double
poisson a
    |a <= 0.94 = -19/20 * log(1- (20*a)/19)
    |otherwise = 3.7



-- 3.
-- Datatype: Stores info on the profile, containing a matrice, a name,
--amount of sequences
data Profile = Profile { matris :: [[(Char,Int)]]
                        , profiltyp :: SekvensTyp
                        , mängdSeq :: Int
                        , profilnamn :: String
                        } deriving(Eq, Show, Read)

-- Returns a profile gived sequences and a name
molseqs2profile :: String -> [MolSeq] -> Profile
molseqs2profile profilnamn molList = Profile (makeProfileMatrix molList) (seqType (head molList)) (length molList) profilnamn

makeProfileMatrix :: [MolSeq] -> [[(Char,Int)]]
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix sl = res
    where
        --Check first element of seq. if dna/protein
        t = seqType (head sl)
        defaults =
            if (t == DNA) then
                -- [(A,0),(C,0),(G,0),(T,0)]
                zip nucleotides (replicate (length nucleotides) 0) -- Rad (i)
            else
                --[(A,0),(R,0),(N,0),(D,0),(C,0),...]
                zip aminoacids (replicate (length aminoacids) 0) -- Rad (ii)
        --applies seqSequence to every MolSeq object in 'sl'
        strs = map seqSequence sl -- Rad (iii)
        --[[('A',3),('C',1),('G',4),..],[('A',1)..]. Transpose groups on each n elements.
        tmp1 = map (map (\x -> ((head x), (length x))) . group . sort)
                   (transpose strs) -- Rad (iv)
        --Making sure all elements, eg.[a,c,g,t] is included if even value = 0
        equalFst a b = (fst a) == (fst b)
        res = map sort (map (\l -> unionBy equalFst l defaults) tmp1)

-- getProfileName
profileName :: Profile -> String
profileName (Profile _ _ _ profilnamn) = profilnamn


-- Finds a the value of a specific position in godycklig matrix.
--Loop list with lists of tuples. Increment i for each list, send x to freqColumn when found.
--FreqColumn iterates over tuples in found list(x) to search for given x(column).
profileFrequency :: Profile -> Int -> Char -> Double
profileFrequency (Profile matris _ mängdSeq _) col row = (fromIntegral(frequencyRad matris 0 row col)) / (fromIntegral mängdSeq)

--Iterates each column in search for the right one, and sends it to freqRad.
frequencyRad :: [[(Char,Int)]] -> Int -> Char -> Int -> Int
frequencyRad (x:xs) i row col
    |i == col = frequencyColumn x row
    |otherwise = frequencyRad xs (i+1) row col

-- takes second element of tuple('A', 1).
frequencyColumn :: [(Char,Int)] -> Char -> Int
frequencyColumn (y:ys) x
    |fst(y) == x = snd(y)
    |otherwise = frequencyColumn ys x




-- distRow loops each row and sends its corresponding columns to distColumn for summation.
-- Divided by # of sequences since we are interested in ratio of difference, not absolute amount.
profileDistance :: Profile -> Profile -> Double
profileDistance profile1 profile2  = distRow (matris profile1) (matris profile2) profile1 profile2


distRow :: [[(Char,Int)]] -> [[(Char,Int)]] -> Profile -> Profile -> Double
distRow [] [] _ _  = 0
distRow (x:xs) (y:ys) pr1 pr2 = (distColumn x y pr1 pr2) + distRow xs ys pr1 pr2

distColumn :: [(Char,Int)] -> [(Char,Int)] -> Profile -> Profile -> Double
distColumn [] [] _ _ = 0
distColumn (x:xs) (y:ys) pr1 pr2 = abs(matris1value - matris2value) + distColumn xs ys pr1 pr2
            where
                matris1value = fromIntegral (snd(x)) / fromIntegral (mängdSeq pr1)
                matris2value = fromIntegral (snd(y)) / fromIntegral (mängdSeq pr2)

-- Evol class object
class Evol object where
    name :: object -> String
    distance :: object -> object -> Double
    distanceMatrix :: [object] -> [(String, String, Double)]
    distanceMatrix [] = []
    distanceMatrix object = helper object object

    helper :: [object] -> [object] -> [(String, String, Double)]
    helper [] [] = []
    helper (x:xs) [] = helper xs xs
    helper (x:xs) (y:ys) = (name x, name y, distance x y) : helper (x:xs) ys

instance Evol MolSeq where
    name = seqName
    distance = seqDistance

instance Evol Profile where
    name = profileName
    distance = profileDistance
