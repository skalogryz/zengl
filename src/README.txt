RU:
Это модифицированная версия ZenGL использующая Direct3D 8/9 в качестве графического API
Версия используемого Direct3D настраивается посредством объявлений в zgl_config.cfg

Отличия от оригинального ZenGL:
- Direct3D9Ex: tex_GetData очень медленная("спасибо" криворуким разработчикам из AMD и Intel). Работает нормально только с render target'ами
- Direct3D8: PR2D_SMOOTH не работает
- Direct3D8: render target'ы не используют альфа-канал ввиду проблем на стороне самого Direct3D8
- render target'ы могут потерять данные внутри после события Device Lost, из-за ограничений Direct3D(окромя Direct3D9Ex)
- нет поддержки сжатых текстур
- 64 битная версия не работает
- scr_SetOptions не возвращает FALSE даже в случаи провала
- не работает сглаживание в оконном режиме(параметр FSAA для zgl_Init)

EN:
This is modified version of ZenGL, that use Direct3D 8/9 as graphics API
Version of Direct3D can be set by defines in zgl_config.cfg

Difference with the original ZenGL:
- Direct3D9Ex: tex_GetData is too slow("thanks" to developers from AMD and Intel). Works normally only with render targets
- Direct3D8: PR2D_SMOOTH doesn't work
- Direct3D8: render targets don't use alpha-chnnel because of bugs in Direct3D8
- render targets can lose data inside after Device Lost, because of Direct3D restrictions(except Direct3D9Ex)
- no support of compressed textures
- 64 bit version doesn't work
- scr_SetOptions won't return FALSE even if it fails
- anti-aliasing doesn't work in windowed mode(FSAA parameter in zgl_Init)