RU:
Это модифицированная версия ZenGL использующая Direct3D 8/9 в качестве графического API
Версия используемого Direct3D настраивается посредством объявлений в zgl_config.cfg

Отличия от оригинального ZenGL:
- Direct3D9Ex: tex_GetData очень медленная("спасибо" криворуким разработчикам из AMD и Intel)
- Direct3D8: PR2D_SMOOTH не работает
- Direct3D8: render target'ы не используют альфа-канал ввиду проблем на стороне самого Direct3D8
- render target'ы могут потерять данные внутри после события Device Lost, из-за ограничений Direct3D
- нет поддержки сжатых текстур
- нет поддержки 64 битной Windows
- scr_SetOptions не возвращает FALSE даже в случаи провала
- не работает сглаживание в оконном режиме(параметр FSAA для zgl_Init)

EN:
This is modified version of ZenGL, that use Direct3D 8/9 as graphics API
Version of Direct3D can be set by defines in zgl_config.cfg

Difference with the original ZenGL:
- Direct3D9Ex: tex_GetData is too slow("thanks" to developers from AMD and Intel)
- Direct3D8: PR2D_SMOOTH doesn't work
- Direct3D8: render targets don't use alpha-chnnel because of bugs in Direct3D8
- render targets can lose data inside after Device Lost, because of Direct3D restrictions
- no support of compressed textures
- no support of Windows 64 bit
- scr_SetOptions won't return FALSE even if it fails
- anti-aliasing doesn't work in windowed mode(FSAA parameter in zgl_Init)