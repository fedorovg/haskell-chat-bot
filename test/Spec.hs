import Test.Hspec

import qualified UtilsSpec
import qualified ConfigSpec
import qualified API.GenericAPISpec
import qualified Telegram.TelegramBotSpec
import qualified Vk.VkBotSpec

main :: IO ()
main = hspec spec

spec = do
    describe "Utils" UtilsSpec.spec
    describe "Configuration" ConfigSpec.spec
    describe "API.GenericAPI" API.GenericAPISpec.spec
    describe "Telegram.TelegramBot" Telegram.TelegramBotSpec.spec
    describe "Vk.VkBotSpec" Vk.VkBotSpec.spec

