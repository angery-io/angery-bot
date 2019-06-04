import os
from telegram.ext import Updater, MessageHandler, Filters
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


def handle_msg(bot, update):
    search_space = update.message.text
    if not (
        search_space
        and not any(suppressor in search_space for suppressor in ANGERY_SUPPRESSOR)
        and any(trigger in search_space for trigger in ANGERY_TRIGGER)
    ):
        return
    with open(ANGERY_RESOURCES, "r") as f:
        imgs = f.readlines()
    img = choice(imgs)[:-1]
    update.message.reply_photo(img)


def main():
    updater = Updater(token=TOKEN)
    dp = updater.dispatcher
    dp.add_handler(MessageHandler(Filters.text, handle_msg))
    updater.start_polling()
    updater.idle()


if __name__ == "__main__":
    main()
