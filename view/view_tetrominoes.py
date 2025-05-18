import psycopg2
from shapely import wkb 
import matplotlib.pyplot as plt
import json

with open('config.json') as f:
    config = json.load(f)

conn = psycopg2.connect(**config)

def show_tetrominoe_by_name(tetro_name):
    cur = conn.cursor()
    cur.execute('''
        SELECT name, color, geom
        FROM tetrominoes
        WHERE name = %s;
    ''', (tetro_name,))
    rows = cur.fetchall()

    for name, color, geom_wkb in rows:
        tetro = wkb.loads(geom_wkb) 

        r, g, b, a = map(int, color.split(','))
        rgba_color = (r / 255, g / 255, b / 255)
        alpha_value = a / 255

        fig, ax = plt.subplots(figsize=(6, 6))
        x, y = tetro.exterior.xy
        ax.fill(x, y, alpha=alpha_value, edgecolor='black', linewidth=5, facecolor=rgba_color)
        ax.plot(x, y, color='black', linewidth=5, alpha=1)

        plt.title('{}'.format(name))
        plt.xlabel('X')
        plt.ylabel('Y')
        plt.xticks(range(0, 7))
        plt.yticks(range(0, 6))
        plt.axis('equal')
        plt.show()

show_tetrominoe_by_name("I")
show_tetrominoe_by_name("J")
show_tetrominoe_by_name("L")
show_tetrominoe_by_name("O")
show_tetrominoe_by_name("S")
show_tetrominoe_by_name("T")
show_tetrominoe_by_name("Z")