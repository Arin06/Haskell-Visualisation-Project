from urllib.parse import urlencode
from urllib.request import Request, urlopen
import json
from PIL import Image


def draw(url, x, y, image_path):
    def set_stone(uid, pos):
        header = {"Content-Type": "text/plain; charset=utf-8"}
        data = {
            'uid': uid,
            'x': pos[0],
            'y': pos[1],
        }     # Set POST fields here
        req = Request(url, json.dumps(data).encode(), header)
        req.get_method = lambda: 'POST'
        response = urlopen(req).read()
        print(response)
    
    im = Image.open(image_path)
    COLS, ROWS  = im.size
    for row in range(ROWS):
        for col in range(COLS):
            r,g,b,a = im.getpixel((col, row))
            if r>240 and g>240 and b>240 or a< 25:
                continue
            else:
                set_stone(
                    0xff000000 & r<<24 |
                    0x00ff0000 & g<<16 |
                    0x0000ff00 & b<<8  |
                    0x000000ff & a
                    ,
                    (x+col, y+row))
    

draw('http://124.221.142.162:8081/doit',
     -800,
     -460,
     "Don.jpg"
    )