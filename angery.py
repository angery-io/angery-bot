#!/usr/bin/python3
import os
import sys
import json
from random import randint
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

print("Content-Type: text/plain")
print()

d = json.load(sys.stdin)
if "challenge" in d:
    print(d["challenge"])
    quit(0)

else:
    event = d.get("event", {})
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
        img = imgs[randint(0, len(imgs) - 1)][:-1]

        payload = (
            '{"attachments": [{"image_url": "%s","fallback": "Angery react"}],"username":"ANGERY Bot","icon_emoji":":angery:","channel":"#general"}'
            % img
        ).encode()
        urlopen(HOOK, data=payload)
