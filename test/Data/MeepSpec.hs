module Data.MeepSpec
  ( spec
  ) where

import           Control.Lens
import           Control.Exception (evaluate)
import           Control.Exception.Lens (_ErrorCall)
import           Test.Hspec.Lens
import           Test.Hspec.QuickCheck

import           Data.Meep (Meep)
import qualified Data.Meep as Meep


spec :: Spec
spec = do
  context "general facts" $
    it "is strict in keys" $
      evaluate (Meep.singleton (error "strict in keys!" :: Bool) 4) `shouldThrow`
        _ErrorCall.only "strict in keys!"

  context "lensy interface" $ do
    it "can insert into an empty Meep" $
      (Meep.empty & at 3 ?~ 'z') `shouldBe` Meep.singleton 3 'z'

    it "can update a non-empty Meep" $
      (Meep.singleton 3 'w' & at 3 ?~ 'z') `shouldBe` Meep.singleton 3 'z'

    it "won't update a non-empty Meep if keys do not match" $
      (Meep.singleton 3 'w' & at 8 ?~ 'z') `shouldBe` Meep.singleton 3 'w'

    it "can wipe a non-empty Meep" $
      (Meep.singleton 3 'w' & at 3 .~ Nothing) `shouldBe` Meep.empty

    it "can successfully lookup a key in a non-empty Meep" $
      (Meep.singleton 3 'w' ^? ix 3) `shouldBe` Just 'w'

    it "can unsuccessfully lookup a key in a non-empty Meep" $
      (Meep.singleton 3 'w' ^? ix 8) `shouldBe` Nothing

    it "can unsuccessfully lookup a key in an empty Meep" $
      (Meep.empty ^? ix 3) `shouldBe` (Nothing :: Maybe Char)

    it "can do lookups with the default value" $
      (Meep.singleton 3 'w' ^. at 4.non 't') `shouldBe` 't'

    it "can do successful member checks" $
      has (ix 3) (Meep.singleton 3 'w') `shouldBe` True

    it "can do unsuccessful member checks" $
      has (ix 8) (Meep.singleton 3 'w') `shouldBe` False

  describe "fromMaybe/toMaybe" $ do
    prop "toMaybe . fromMaybe is an identity" $ \x ->
      (Meep.toMaybe . Meep.fromMaybe) x == (x :: Maybe (Int, Char))

    prop "fromMaybe . toMaybe is an identity" $ \x ->
      (Meep.fromMaybe . Meep.toMaybe) x == (x :: Meep Int Char)
