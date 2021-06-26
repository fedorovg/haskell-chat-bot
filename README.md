# Haskell Chat Bot

Point of this project was to write a chat bot in pure haskell without any platform-specific wrapper libraries.
After receiving a message this bot retrieves data from extrernal API specified in the config and responds with a formated message.

This bot is written in more or less generic fashion and is able to work with multiple messaging platforms and Data APIs based on a config file.

### Bot supports 2 platforms: Telegram and Vk

Vk and telegram were the only sane options, because all other platforms have really comlpex APIs, that are intended to be used with a wrapper library (and with a wrapper, project would have been to easy).

### Platform and data source are lodaded based on config.

Bot reads a JSON file a startup.

###  Data sources:

1. A hardcoded client (`API/Weather.hs`) that comunicates with OpenWeatherAPI.
2. Generic API client (`API/GenericAPI.hs`), that works with whatever that is defined inside the config file.

## GenericAPI

Since this client isn't meant to work with any particular API, I can't write a formater for API's response, because I have no idea, what I will actually receive. That's why GenericAPI converts JSON it received from a service to yaml, which is argaubly more readable, and then sends it as a response to a user.

