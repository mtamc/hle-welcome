module Main (main) where

import App                (discordTok, mkEnv, run)
import App.Discord.Events (onDiscordEvent)
import App.Discord.Lenses
import Control.Lens       ((.~))
import Discord            (def, runDiscord)

main ∷ IO ()
main = do
  echo "Bot started."
  env ← mkEnv
  botTerminationError ← runDiscord $ def
    & token         .~ discordTok env
    & gatewayIntent .~ (def & members .~ True & presences .~ True)
    & onEvent       .~ App.run env . onDiscordEvent
  echo $ "A fatal error occurred: " ⊕ botTerminationError
