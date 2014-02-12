#define PASTE(a,b) a##b
#ifndef MIN
#   define __MIN(a,b,c) ({ \
        __typeof(a) const PASTE(__a,c) = (a); \
        __typeof(b) const PASTE(__b,c) = (b); \
        (__typeof(a))(PASTE(__a,c) > PASTE(__b,c) ? PASTE(__b,c) : PASTE(__a,c)); \
    })
#   define MIN(a,b) __MIN((a), (b), __COUNTER__)
#endif
#ifndef MAX
#   define __MAX(a,b,c) ({ \
        __typeof(a) const PASTE(__a,c) = (a); \
        __typeof(b) const PASTE(__b,c) = (b); \
        PASTE(__a,c) > PASTE(__b,c) ? PASTE(__a,c) : PASTE(__b,c); \
    })
#   define MAX(a,b) __MAX((a), (b), __COUNTER__)
#endif

#ifndef BETWEEN
#   define __BETWEEN(val, low, high, c) ({ \
        __typeof(val) const PASTE(__val,c) = (val); \
        PASTE(__val,c) > (low) && PASTE(__val,c) < (high); \
    })
#   define BETWEEN(val, low, high) __BETWEEN((val), (low), (high), __COUNTER__)
#endif
#ifndef INRANGE
#   define __INRANGE(val, low, high, c) ({ \
        __typeof(val) const PASTE(__val,c) = (val); \
        PASTE(__val,c) >= (low) && PASTE(__val,c) <= (high); \
    })
#   define INRANGE(val, low, high) __INRANGE((val), (low), (high), __COUNTER__)
#endif
#ifndef CLAMP
#   define CLAMP(val, min, max) MAX((min), MIN((val), (max)))
#endif

#define POWOF2(n) ({ __typeof(n) __n = (n); (__n != 0) && !(__n & (__n - 1)); })

#define DUFF(count, action...) do { \
    typeof(count) count_ = (count); \
    typeof(count) times_ = (count_ + 15) >> 4; \
    switch(count_&7) { \
        case 0: do { action; \
        case 15: action; \
        case 14: action; \
        case 13: action; \
        case 12: action; \
        case 11: action; \
        case 10: action; \
        case 9: action; \
        case 8: action; \
        case 7: action; \
        case 6: action; \
        case 5: action; \
        case 4: action; \
        case 3: action; \
        case 2: action; \
        case 1: action; \
    } while(--times_ > 0); \
    } \
}while(0)

