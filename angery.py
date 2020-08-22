#!/usr/bin/python3
import os
import sys
import json
from random import choice
from urllib.request import urlopen, Request
from urllib.parse import parse_qsl, urlencode
import hmac
import hashlib
from time import time
import boto3


SLACK_OAUTH_API = "https://slack.com/api/oauth.access"
SLACK_POSTMESSAGE_API = "https://slack.com/api/chat.postMessage"
SLACK_USER_API = "https://slack.com/api/users.info?"
SLACK_TIMESTAMP = "X-Slack-Request-Timestamp"
SLACK_SIGNATURE = "X-Slack-Signature"

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

SDB_DOMAIN = "BOT_TOKENS"


def get_sdb(singleton=[]):
    if not singleton:
        singleton.append(boto3.client("sdb", region_name="eu-west-1"))
    return singleton[0]


def verify_request_comes_from_slack(event):
    try:
        SIGNING_SECRET = os.environ["SLACK_SIGNING_SECRET"].encode()
    except KeyError:
        raise RuntimeError("SLACK_SIGNING_SECRET environment variable is not set.")

    headers = event.get("headers") or {}
    if SLACK_TIMESTAMP not in headers or SLACK_SIGNATURE not in headers:
        raise RuntimeError("Missing Slack verification headers")

    timestamp = headers[SLACK_TIMESTAMP]
    if time() - int(timestamp) > 60:
        raise RuntimeError("Slack message too old - possible replay")

    raw_body = event["body"]
    signature_base = f"v0:{timestamp}:{raw_body}".encode()
    signature = (
        "v0=" + hmac.new(SIGNING_SECRET, signature_base, hashlib.sha256).hexdigest()
    )
    if signature != headers[SLACK_SIGNATURE]:
        raise RuntimeError("Slack signature not matching")


def put_attributes(sdb, domain, team_id, token=None, reacts=True, **kwargs):
    attributes = [
        {"Name": name, "Value": value, "Replace": True}
        for name, value in (
            ([("token", token)] if token else [])
            + [("reacts", str(reacts))]
            + list(kwargs.items())
        )
    ]
    response = sdb.put_attributes(
        DomainName=domain, ItemName=team_id, Attributes=attributes
    )


def get_attributes(sdb, domain, team_id):
    response = sdb.get_attributes(DomainName=domain, ItemName=team_id)
    return {attr["Name"]: attr["Value"] for attr in response.get("Attributes", [])}


def register(params):
    try:
        CLIENT_ID = os.environ["CLIENT_ID"]
        CLIENT_SECRET = os.environ["CLIENT_SECRET"]
    except KeyError:
        raise RuntimeError("Slack client id or secret environment variable is not set.")

    if "error" in params:
        return

    code = params.get("code")
    if not code:
        raise RuntimeError("Malformed register request")

    data = f"client_id={CLIENT_ID}&client_secret={CLIENT_SECRET}&code={code}"
    request = Request(
        SLACK_OAUTH_API,
        method="POST",
        data=data.encode(),
        headers={"Content-type": "application/x-www-form-urlencoded"},
    )
    with urlopen(request) as response:
        auth_details = json.load(response)

    team_id = auth_details.get("team_id")
    team_name = auth_details.get("team_name")
    bot_token = auth_details.get("bot", {}).get("bot_access_token")
    if not auth_details.get("ok") or not team_id or not bot_token:
        raise RuntimeError("Error getting auth tokens")

    put_attributes(get_sdb(), SDB_DOMAIN, team_id, bot_token, team_name=team_name)


def check_admin(user_id, bot_token):
    data = {"token": bot_token, "user": user_id}
    request = Request(SLACK_USER_API + urlencode(data))
    with urlopen(request) as response:
        user = json.load(response)
    if not user.get("ok"):
        raise RuntimeError("Something wrong with user api")
    return user["user"]["is_admin"]


def slash_command(body):
    if body.get("command") == "/angery-reacts":
        if body.get("text") not in ("activate", "deactivate"):
            return "Not sure what you mean :angry:"

        sdb = get_sdb()
        team_id = body["team_id"]
        attributes = get_attributes(sdb, SDB_DOMAIN, team_id)
        if not check_admin(body["user_id"], attributes["token"]):
            return "I don't talk to you non-admin folk"

        if body["text"] == "deactivate":
            put_attributes(sdb, SDB_DOMAIN, team_id, reacts=False)
            return "Angery reacts deactivated"
        else:
            put_attributes(sdb, SDB_DOMAIN, team_id, reacts=True)
            return "Angery reacts activated"
    else:
        return None


def handle_message(body):
    if "challenge" in body:
        return {"body": body["challenge"]}

    team_id = body.get("team_id")
    event = body.get("event", {})
    channel = event.get("channel", "")
    text = event.get("text", "").lower()
    item = event.get("item", {})
    reaction = event.get("reaction", "")
    channel = channel or item.get("channel", "")
    search_space = text + reaction

    if not (
        search_space
        and not any(suppressor in search_space for suppressor in ANGERY_SUPPRESSOR)
        and any(trigger in search_space for trigger in ANGERY_TRIGGER)
    ):
        return {}
        
    with open(ANGERY_RESOURCES, "r") as f:
        imgs = f.readlines()
    img = choice(imgs)[:-1]

    sdb = get_sdb()
    attributes = get_attributes(sdb, SDB_DOMAIN, team_id)
    token = attributes.get("token")
    if not token:
        raise RuntimeError(f"No token for team_id: {team_id}")
    if reaction and attributes.get("reacts", "True") != "True":
        return {}
    payload = {
        "channel": channel,
        "attachments": [{"image_url": img, "fallback": "Angery react"}],
    }
    encoded = json.dumps(payload).encode()
    request = Request(
        SLACK_POSTMESSAGE_API,
        data=encoded,
        headers={
            "Content-type": "application/json",
            "Authorization": f"Bearer {token}",
        },
        method="POST",
    )
    urlopen(request)
    return {}


def lambda_handler(event, context):
    if event.get("resource") == "/AngeryBot/register":
        register(event.get("queryStringParameters", {}))
        try:
            SLACK_APP_ID = os.environ["SLACK_APP_ID"]
            return {
                "statusCode": 302,
                "headers": {"Location": f"https://slack.com/apps/{SLACK_APP_ID}"},
            }
        except KeyError:
            return {
                "body": "We will now deliver your angeries - don't forget to invite the Angery bot to your channels!"
            }

    elif event.get("resource") == "/AngeryBot/users":
        sdb = get_sdb()
        items = sdb.select(SelectExpression=f"select team_name from {SDB_DOMAIN}").get(
            "Items", []
        )
        response = {"users": [i["Attributes"][0]["Value"] for i in items]}
        response["count"] = len(response["users"])
        return {"body": json.dumps(response)}
    elif event.get("resource") == "/AngeryBot/command":
        verify_request_comes_from_slack(event)
        response = slash_command(dict(parse_qsl(event["body"])))
        return {"statusCode": 200 if response else 400, "body": response}

    verify_request_comes_from_slack(event)
    body = json.loads(event["body"])
    if body.get("event", {}).get("type") == "app_uninstalled":
        get_sdb().delete_attributes(DomainName=SDB_DOMAIN, ItemName=body["team_id"])
    else:
        return handle_message(body)
    return {}
