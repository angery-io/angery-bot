#!/usr/bin/python3
import os
import sys
import json
from random import choice
from urllib.request import urlopen


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


try:
    HOOK = os.environ["SLACK_HOOK"]
except KeyError:
    raise RuntimeError("SLACK_HOOK environment variable is not set.")


def lambda_handler(event, context):
    if "challenge" in event:
        return

    else:
        event = event.get("event", {})
        channel = event.get("channel", "")
        text = event.get("text", "").lower()
        item = event.get("item", {})
        reaction = event.get("reaction", "")
        channel = channel or item.get("channel", "")
        search_space = text + reaction

        if (
            search_space
            and not any(suppressor in search_space for suppressor in ANGERY_SUPPRESSOR)
            and any(trigger in search_space for trigger in ANGERY_TRIGGER)
        ):
            with open(ANGERY_RESOURCES, "r") as f:
                imgs = f.readlines()
            img = choice(imgs)[:-1]

            payload = (
                '{"attachments": [{"image_url": "%s","fallback": "Angery react"}],"username":"ANGERY Bot","icon_emoji":":angery:","channel":"#general"}'
                % img
            ).encode()
            urlopen(HOOK, data=payload)
