from libqtile.layout.base import _SimpleLayoutBase


class DistractionFree(_SimpleLayoutBase):
    """Distraction Free layout (a.k.a centerfocus)
    A single focused window with empty space around it.
    """

    defaults = [
        ("margin", 5, "Margin of the layout (int or list of ints [N E S W])"),
        ("border_focus", "#0000ff", "Border colour(s) for the window when focused"),
        ("border_normal", "#000000", "Border colour(s) for the window when not focused"),
        ("border_width", 0, "Border width."),
        ("sizing_mode", "absolute", "Method to determine the size of the window rect: absolute or relative."),
        ("width", 2560, "The window rect's width."),
        ("height", 1200, "The window rect's height."),
        ("grow_amount", 100, "Amount by which to grow/shrink the window."),
    ]

    def __init__(self, **config):
        _SimpleLayoutBase.__init__(self, **config)
        self.add_defaults(DistractionFree.defaults)

    def add_client(self, client):
        return super().add_client(client, 1)

    def configure(self, client, screen_rect):
        if self.clients and client is self.clients.current_client:
            w = (screen_rect.width * self.width if self.sizing_mode == "relative" else self.width) - self.border_width * 2
            h = (screen_rect.height * self.height if self.sizing_mode == "relative" else self.height) - self.border_width * 2

            client.place(
                screen_rect.x + int((screen_rect.width - w) / 2),
                screen_rect.y + int((screen_rect.height - h) / 2),
                w,
                h,
                self.border_width,
                self.border_focus if client.has_focus else self.border_normal,
                margin=self.margin,
            )
            client.unhide()
        else:
            client.hide()

    def cmd_up(self):
        _SimpleLayoutBase.previous(self)

    def cmd_down(self):
        _SimpleLayoutBase.next(self)

    def cmd_resize(self, resize_by = (0, 0)):
        self.width = self.width + resize_by[0]
        self.height = self.height + resize_by[1]
        self.group.layout_all()

    def cmd_grow_h(self):
        self.resize((self.grow_amount, 0))

    def cmd_grow_v(self):
        self.resize((0, self.grow_amount))

    def cmd_shrink_h(self):
        self.resize((-self.grow_amount, 0))

    def cmd_shrink_v(self):
        self.resize((0, -self.grow_amount))
