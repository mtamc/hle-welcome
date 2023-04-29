module App (App, Cfg (..), Env (..), discordTok, gId, memberRole, mkEnv, run, welcomeCh) where

import Data.Map             qualified as Map
import Dhall                (FromDhall, auto, input)
import Discord              (DiscordHandler)
import Discord.Types        (ChannelId, DiscordId (DiscordId), GuildId, MessageId, RoleId,
                             Snowflake (Snowflake), UserId)
import Relude.Extra.Newtype (un)

type App a = ReaderT Env DiscordHandler a

data Env
  = Env
    { cfg             ∷ Cfg
    , pendingDeletion ∷ MVar (Map UserId MessageId)
    }
  deriving (Generic)

data Cfg
  = Cfg
    { discordBotToken  ∷ Text
    , welcomeChannelId ∷ Word64
    , guildId          ∷ Word64
    , memberRoleId     ∷ Word64
    , secretWord       ∷ Text
    , welcomeMsg       ∷ Text
    , confirmationMsg  ∷ Text
    }
  deriving (Generic, Show)
instance FromDhall Cfg

mkEnv ∷ MonadIO m ⇒ m Env
mkEnv = Env
  <$> liftIO (input auto "./config.dhall")
  <*> newMVar Map.empty

run ∷ Env → App a → DiscordHandler a
run env = usingReaderT env . un

discordTok ∷ Env → Text
discordTok env = env.cfg.discordBotToken

welcomeCh ∷ Env → ChannelId
welcomeCh env = dId env.cfg.welcomeChannelId

gId ∷ Env → GuildId
gId env = dId env.cfg.guildId

memberRole ∷ Env → RoleId
memberRole env = dId env.cfg.memberRoleId

dId ∷ Word64 → DiscordId a
dId = DiscordId . Snowflake
