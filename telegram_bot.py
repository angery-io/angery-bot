import os
import telegram
from random import choice

try:
    TOKEN = os.environ["TOKEN"]
except KeyError:
    raise RuntimeError("TOKEN not set")


ANGERY_SUPPRESSOR = ("no_react", "noreact", "nobot", "no_bot")
ANGERY_TRIGGER = (
    "angery",
    "angry",
    "anger",
    ":frowning:",
    ":anger:",
    ":angry:",
    "aggravating",
)
ANGERY_RESOURCES = os.path.join("resources", "angery.txt")
BOT = telegram.Bot(token=TOKEN)


def lambda_handler(event, context):
    body = event["body"]
    update = telegram.Update.de_json(body, BOT)
    search_space = update.message.text
    if not (
        search_space
        and not any(suppressor in search_space for suppressor in ANGERY_SUPPRESSOR)
        and any(trigger in search_space for trigger in ANGERY_TRIGGER)
    ):
        return {}
    with open(ANGERY_RESOURCES, "r") as f:
        imgs = f.readlines()
    img = choice(imgs)[:-1]
    update.message.reply_photo(img)
    return {}
