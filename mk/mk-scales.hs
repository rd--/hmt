import Music.Theory.Tuning
import Music.Theory.Tuning.Scala

prj_dir :: FilePath
prj_dir = "/home/rohan/sw/hmt/"

prj_file :: FilePath -> FilePath
prj_file = (++) prj_dir

-- > mapM_ gen_et_scl [12,19,31,53,72,96]
gen_et_scl :: Int -> IO ()
gen_et_scl n = do
  let nm = "et" ++ show n
      dsc = show n ++ " tone equal temperament"
      scl = tuning_to_scale (nm,dsc) (equal_temperament n)
  writeFile (prj_file ("data/scl/" ++ nm ++ ".scl")) (unlines (scale_pp scl))
