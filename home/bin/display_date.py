#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Display date in dwm's status line


import subprocess
import time
from datetime import datetime

from Xlib.display import Display

r = Display().screen().root

while True:
    battery = subprocess.check_output(
        "upower --show-info $(upower --enumerate | grep BAT) | grep percentage | grep -o \"[0-9]\+\" | head -1",
        shell = True,
        stderr = subprocess.STDOUT)[:-1]
    currtime = datetime.now().strftime(' CW %V, %a %b %e %T %Y ')
    status_line = f" Bat: {battery}% {currtime}"

    r.set_wm_name(status_line)
    r.get_wm_name()
    time.sleep(1)
