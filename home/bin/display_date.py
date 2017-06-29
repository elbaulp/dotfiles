#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Display date in dwm's status line


from Xlib.display import Display
from datetime import datetime
import time

r = Display().screen().root

while True:
    r.set_wm_name(datetime.now().strftime(u' CW %V, %a %b %e %T %Y '))
    r.get_wm_name()
    time.sleep(1)
