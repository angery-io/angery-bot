import os
import telegram
import json
from random import choice

try:
    TOKEN = os.environ["TOKEN"]
except KeyError:
    raise RuntimeError("TOKEN not set")


NOT_ANGERY = ["arranger", "danger", "hanger", "manger", "ranger", "tangerine"]
ANGERY_SUPPRESSOR = ["no_react", "noreact", "nobot", "no_bot"] + NOT_ANGERY
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
    body = json.loads(event["body"])
    update = telegram.Update.de_json(body, BOT)
    if not update or not update.message or not update.message.text:
        return {}
    search_space = update.message.text.lower()
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
    write_angery_metric()
    return {}


def write_angery_metric():
    cloudwatch = boto3.client("cloudwatch")
    response = cloudwatch.put_metric_data(
        Namespace="AngeryBot",
        MetricData=[{"MetricName": "AngeryServed", "Unit": "Count", "Value": 1}],
    )
