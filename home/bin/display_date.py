#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Display date in dwm's status line


from Xlib.display import Display
from datetime import datetime
import subprocess
import time

r = Display().screen().root

while True:
    battery = subprocess.check_output(
        "upower --show-info $(upower --enumerate | grep BAT) | grep percentage | grep -o \"[0-9]\+\" | head -1",
        shell = True,
        stderr = subprocess.STDOUT)[:-1]
    currtime = datetime.now().strftime(u' CW %V, %a %b %e %T %Y ')
    status_line = " Bat: {0}% {1}".format(battery, currtime)

    r.set_wm_name(status_line)
    r.get_wm_name()
    time.sleep(1)
