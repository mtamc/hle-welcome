module App.Discord.Events (onDiscordEvent) where

import App                (App, Cfg (..), Env (..), gId, memberRole, welcomeCh)
import Control.Concurrent (modifyMVar_, threadDelay)
import Data.Map           qualified as Map
import Data.Text          qualified as Text
import Discord            (restCall)
import Discord.Requests   qualified as R
import Discord.Types      (ChannelId, DiscordId (unId), Event (..), GuildId, GuildMember (..),
                           Message (..), Snowflake (unSnowflake), User (..))

onDiscordEvent ∷ Event → App ()
onDiscordEvent = \case
  GuildMemberAdd guildId member → onUserJoin guildId member
  MessageCreate msg             → onMsg msg
  _                             → pass

onUserJoin ∷ GuildId → GuildMember → App ()
onUserJoin guildId member = do
  env ← ask
  case (member.memberUser <&> userId, guildId ≡ gId env) of
    (Just uid, True) → do
      let uidText = show . unSnowflake $ unId uid
      botMsg ← sendMsg (welcomeCh env)
        ("<@" ⊕ uidText ⊕ "> " ⊕ env.cfg.welcomeMsg)
      liftIO $ modifyMVar_ env.pendingDeletion (pure . Map.insert uid botMsg.messageId)
    _ →
      pass

onMsg ∷ Message → App ()
onMsg msg = do
  env ← ask
  let hasSecretWord = env.cfg.secretWord `Text.isInfixOf` Text.toLower msg.messageContent
      uid = userId msg.messageAuthor
      uidText = show . unSnowflake $ unId uid
  case (hasSecretWord, msg.messageChannelId ≡ welcomeCh env) of
    (True, True) → do
      void . lift . restCall $ R.DeleteMessage (welcomeCh env, msg.messageId)
      void . lift . restCall
        $ R.AddGuildMemberRole (gId env) (userId msg.messageAuthor) (memberRole env)
      msgsPendingDeletion ← readMVar env.pendingDeletion
      case Map.lookup (userId msg.messageAuthor) msgsPendingDeletion of
        Nothing → pass
        Just msgIdToDelete → do
          void . lift . restCall $ R.DeleteMessage (welcomeCh env, msgIdToDelete)
          liftIO $ modifyMVar_ env.pendingDeletion (pure . Map.delete uid)
          botMsg ← sendMsg (welcomeCh env)
            ("<@" ⊕ uidText ⊕ "> " ⊕ env.cfg.confirmationMsg)
          liftIO $ threadDelay 120_000_000 -- 2 minutes / 120 secs
          void . lift . restCall $ R.DeleteMessage (welcomeCh env, botMsg.messageId)
    _ →
      pass

sendMsg ∷ ChannelId → Text → App Message
sendMsg ch txt = lift do
  result ← restCall $ R.CreateMessage ch txt
  case result of
    Left _    → fail "Failed to reply"
    Right msg → pure msg
