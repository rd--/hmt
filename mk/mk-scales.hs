import qualified Music.Theory.Tuning as T {- hmt -}
import qualified Music.Theory.Tuning.Scala as S {- hmt -}

prj_dir :: FilePath
prj_dir = "/home/rohan/sw/hmt/"

prj_file :: FilePath -> FilePath
prj_file = (++) prj_dir

-- > mapM_ gen_et_scl [12,19,31,53,72,96]
gen_et_scl :: Int -> IO ()
gen_et_scl n = do
  let nm = "et" ++ show n
      dsc = show n ++ " tone equal temperament"
      scl = S.tuning_to_scale (nm,dsc) (T.equal_temperament n)
  writeFile (prj_file ("data/scl/" ++ nm ++ ".scl")) (unlines (S.scale_pp scl))

-- > mapM_ gen_hs_scl [17,19,21,23]
gen_hs_scl :: Integer -> IO ()
gen_hs_scl n = do
  let nm = "hs" ++ show n
      dsc = show n ++ " tone harmonic series"
      scl = S.tuning_to_scale (nm,dsc) (T.harmonic_series n 2)
  writeFile (prj_file ("data/scl/" ++ nm ++ ".scl")) (unlines (S.scale_pp scl))
