#!/usr/bin/env python3

import subprocess
import webbrowser

# List of bookmarks
bookmarks = [
    (
        "work-timesheet",
        "https://docs.google.com/spreadsheets/d/1zncWCq9d1nZMX2thhDO4UD8JSCJoJFJVvnkdM7K4G20/edit?gid=1732172782#gid=1732172782",
    ),
    ("kiro", "https://elearning.unipv.it/"),
    ("gmail", "https://mail.google.com/mail/u/0/"),
    ("drive", "https://drive.google.com/drive/u/0/my-drive"),
    ("nextcloud", "https://nextcloud.s3m.dev/index.php/login"),
    ("youtube", "https://www.youtube.com/"),
    ("commercialista", "https://fidocommercialista.it/"),
    ("ynab", "https://www.ynab.com/"),
]


def select_bookmark():
    # Create a list of bookmark names
    names = [bookmark[0] for bookmark in bookmarks]

    # Use dmenu to select a bookmark name
    process = subprocess.Popen(["tofi"], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    selection, _ = process.communicate(input="\n".join(names).encode("utf-8"))

    # Decode the selection
    selection = selection.decode("utf-8").strip()

    return selection


def open_bookmark(name):
    # Find the URL corresponding to the selected bookmark name
    url = next((url for bookmark_name, url in bookmarks if bookmark_name == name), None)

    if url:
        webbrowser.open(url)


if __name__ == "__main__":
    selected_name = select_bookmark()

    if selected_name:
        open_bookmark(selected_name)
