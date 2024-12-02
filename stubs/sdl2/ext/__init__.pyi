from sdl2 import SDL_Event, SDL_Surface

def init() -> None:
	...

class Surface:
	...

class Window:
	def __init__(
		self, title: str, size: tuple[int, int],
		position: tuple[int, int] | None=...,
		flags: int | None=...
	) -> None:
		...

	def get_surface(self) -> SDL_Surface:
		...

	def show(self) -> None:
		...

	def refresh(self) -> None:
		...

def get_events() -> list[SDL_Event]:
	...