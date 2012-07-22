RU:
Это модифицированная версия ZenGL использующая Direct3D 8/9 в качестве графического API
Версия используемого Direct3D настраивается посредством объявлений в zgl_config.cfg

Отличия от оригинального ZenGL:
- нет поддержки сжатых текстур
- tex_SetData не работает для текстур используемых в качестве Render Target'а(есть только в режиме Direct3D9 Ex)
- нет поддержки 64 битной Windows
- scr_SetOptions не возвращает FALSE даже в случаи провала

EN:
This is modified version of ZenGL, that use Direct3D 8/9 as graphics API
Version of Direct3D can be set by defines in zgl_config.cfg

Difference with the original ZenGL:
- no support of compressed textures
- tex_SetData doesn't work with textures, that was used like Render Target(available only in Direct3D9 Ex mode)
- no support of Windows 64 bit
- scr_SetOptions won't return FALSE even if it fails