#!/usr/bin/env python
import os
import subprocess
import sys

import conanfile

remote = "gitlab"


def run_conan(args):
    cmd = ["conan"] + args
    print("running: %s" % " ".join(cmd))
    sys.stdout.flush()
    subprocess.check_call(cmd)


def authenticate():
    token = os.environ.get("CI_JOB_TOKEN")
    if token:
        run_conan(
            ["user", "--password={}".format(token), "-r", remote, "ci_user"]
        )


def build():
    api_url = "%s/packages/conan" % os.environ.get(
        "CI_API_V4_URL", "https://git.grammatech.com/api/v4"
    )
    try:
        run_conan(["remote", "add", remote, api_url])
    except subprocess.CalledProcessError:
        pass  # ignore, maybe already added?

    props = conanfile.Properties()
    authenticate()
    run_conan(["create"] + sys.argv[1:] + [".", props.conan_ref])
    archived_channels = props.archived_channels
    if props.conan_channel in archived_channels:
        run_conan(["upload", props.conan_recipe, "--all", "--remote", remote])
    else:
        print(
            "Conan channel not archived. Update archived_branches"
            " in conanfile.py to get archival."
        )
        print("archived channels: ")
        print(*archived_channels, sep=", ")
        print("channel built: " + props.conan_channel)


if __name__ == "__main__":
    build()
