-- | United miscellaneous functionality.

module Pos.Core.Block.Union.Instances
       ( getBlockHeader
       , blockHeader
       ) where

import           Universum

import           Control.Lens (Getter, choosing, lens, to)
import qualified Data.Text.Buildable as Buildable

import           Pos.Binary.Class (Bi)
import           Pos.Core.Block.Blockchain (GenericBlock (..))
import           Pos.Core.Block.Genesis ()
import           Pos.Core.Block.Main ()
import           Pos.Core.Block.Union.Types (Block, BlockHeader, ComponentBlock (..),
                                             blockHeaderHash)
import           Pos.Core.Class (HasBlockVersion (..), HasDifficulty (..), HasEpochIndex,
                                 HasEpochOrSlot (..), HasHeaderHash (..), HasPrevBlock (..),
                                 HasSoftwareVersion (..), IsHeader, IsMainHeader (..), epochIndexL)

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

instance Bi BlockHeader =>
         Buildable BlockHeader where
    build = either Buildable.build Buildable.build

----------------------------------------------------------------------------
-- HasHeaderHash
----------------------------------------------------------------------------

instance Bi BlockHeader =>
         HasHeaderHash BlockHeader where
    headerHash = blockHeaderHash

instance Bi BlockHeader =>
         HasHeaderHash Block where
    headerHash = blockHeaderHash . getBlockHeader

-- | Take 'BlockHeader' from either 'GenesisBlock' or 'MainBlock'.
getBlockHeader :: Block -> BlockHeader
getBlockHeader = bimap _gbHeader _gbHeader

blockHeader :: Getter Block BlockHeader
blockHeader = to getBlockHeader

----------------------------------------------------------------------------
-- HasDifficulty
----------------------------------------------------------------------------

instance HasDifficulty BlockHeader where
    difficultyL = choosing difficultyL difficultyL

instance HasDifficulty Block where
    difficultyL = choosing difficultyL difficultyL

instance HasDifficulty (ComponentBlock a) where
    difficultyL = difficultyL

----------------------------------------------------------------------------
-- IsHeader
----------------------------------------------------------------------------

instance Bi BlockHeader  => IsHeader BlockHeader
instance IsHeader (ComponentBlock a)

----------------------------------------------------------------------------
-- HasEpochIndex
----------------------------------------------------------------------------

instance HasEpochIndex (ComponentBlock a) where
    epochIndexL = lens getter setter
      where
        getter (ComponentBlockGenesis genesisHeader) =
            genesisHeader ^. epochIndexL
        getter (ComponentBlockMain mainHeader _) = mainHeader ^. epochIndexL
        setter (ComponentBlockGenesis genesisHeader) e =
            ComponentBlockGenesis (genesisHeader & epochIndexL .~ e)
        setter (ComponentBlockMain mainHeader payload) e =
            ComponentBlockMain (mainHeader & epochIndexL .~ e) payload

----------------------------------------------------------------------------
-- IsMainHeader
----------------------------------------------------------------------------

instance IsMainHeader (ComponentBlock a) where
    headerSlotL = headerSlotL
    headerLeaderKeyL = headerLeaderKeyL

----------------------------------------------------------------------------
-- HasEpochOrSlot
----------------------------------------------------------------------------

instance HasEpochOrSlot (ComponentBlock a) where
    getEpochOrSlot  = getEpochOrSlot

----------------------------------------------------------------------------
-- HasPrevBlock
----------------------------------------------------------------------------

instance HasPrevBlock (ComponentBlock a) where
    prevBlockL = prevBlockL

----------------------------------------------------------------------------
-- HasHeaderHash
----------------------------------------------------------------------------

instance HasHeaderHash (ComponentBlock a) where
    headerHash = headerHash

----------------------------------------------------------------------------
-- HasBlockVersion
----------------------------------------------------------------------------

instance HasBlockVersion (ComponentBlock a) where
    blockVersionL = blockVersionL

----------------------------------------------------------------------------
-- HasSoftwareVersion
----------------------------------------------------------------------------

instance HasSoftwareVersion (ComponentBlock a) where
    softwareVersionL = softwareVersionL
