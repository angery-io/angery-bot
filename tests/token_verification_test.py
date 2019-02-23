import copy
import pytest
from angery import angery


# https://api.slack.com/docs/verifying-requests-from-slack
SIGNING_SECRET = "8f742231b10e8888abcd99yyyzzz85a5"
REQUEST_BODY = "token=xyzz0WbapA4vBCDEFasx0q6G&team_id=T1DC2JH3J&team_domain=testteamnow&channel_id=G8PSS9T3V&channel_name=foobar&user_id=U2CERLKJA&user_name=roadrunner&command=%2Fwebhook-collect&text=&response_url=https%3A%2F%2Fhooks.slack.com%2Fcommands%2FT1DC2JH3J%2F397700885554%2F96rGlfmibIGlgcZRskXaIFfN&trigger_id=398738663015.47445629121.803a0bc887a14d10d2c447fce8b6703c"
TIMESTAMP = 1531420618
SIGNATURE_HEADER = "v0=a2114d57b48eac39b9ad189dd8316235a7b4a8d21a10bd27519666489c69b503"
TOKEN_TEST_EVENT = {
    "headers": {
        "X-Slack-Signature": SIGNATURE_HEADER,
        "X-Slack-Request-Timestamp": TIMESTAMP,
    },
    "body": REQUEST_BODY,
}


def test_token_verification_success(monkeypatch):
    with monkeypatch.context() as m:
        m.setenv("SLACK_SIGNING_SECRET", SIGNING_SECRET)
        # I don't know why. but this works
        m.setattr("angery.angery.time", lambda: TIMESTAMP)
        angery.verify_request_comes_from_slack(TOKEN_TEST_EVENT)


def test_token_verification_too_old(monkeypatch):
    with monkeypatch.context() as m:
        m.setenv("SLACK_SIGNING_SECRET", SIGNING_SECRET)
        # I don't know why. but this works
        m.setattr("angery.angery.time", lambda: TIMESTAMP + 1000)
        with pytest.raises(RuntimeError):
            angery.verify_request_comes_from_slack(TOKEN_TEST_EVENT)


def test_token_verification_incorrect_signature(monkeypatch):
    token_test_event = copy.deepcopy(TOKEN_TEST_EVENT)
    token_test_event["headers"]["X-Slack-Signature"] = "Something else"
    with monkeypatch.context() as m:
        m.setenv("SLACK_SIGNING_SECRET", SIGNING_SECRET)
        # I don't know why. but this works
        m.setattr("angery.angery.time", lambda: TIMESTAMP)
        with pytest.raises(RuntimeError):
            angery.verify_request_comes_from_slack(token_test_event)


def test_token_verification_missing_signature(monkeypatch):
    token_test_event = copy.deepcopy(TOKEN_TEST_EVENT)
    del token_test_event["headers"]["X-Slack-Signature"]
    with monkeypatch.context() as m:
        m.setenv("SLACK_SIGNING_SECRET", SIGNING_SECRET)
        # I don't know why. but this works
        m.setattr("angery.angery.time", lambda: TIMESTAMP)
        with pytest.raises(RuntimeError):
            angery.verify_request_comes_from_slack(token_test_event)
