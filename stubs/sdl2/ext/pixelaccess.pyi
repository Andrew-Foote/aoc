import numpy as np
from sdl2.ext import SoftwareSprite
from sdl2.surface import SDL_Surface

Surface = SDL_Surface | SoftwareSprite

def pixels2d(source: Surface, transpose: bool=...) -> np.ndarray:
	...