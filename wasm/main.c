#include <malloc.h>
#include <stdio.h>
#include <stdarg.h>
#include <math.h>

#define true 1
#define false 0

typedef struct tick_template {
    float height;
    char* (*show)(float);
} tick_template;

tick_template* new_template (float height, char* (*show)(float)) {
    tick_template* t = malloc(sizeof(tick_template));
    t->height = height;
    t->show = show;
    return t;
}

char* prec1 (float f) {
    char* buf = malloc(sizeof(char)*100);
    sprintf(buf, "%.1f", f);
    return buf;
}

char* prec0 (float f) {
    char* buf = malloc(sizeof(char)*100);
    sprintf(buf, "%.0f", roundf(f));
    return buf;
}

tick_template* new_template_prec0 (float height) {
    return new_template(height, *prec0);
}

tick_template* new_template_prec1 (float height) {
    return new_template(height, *prec1);
}

typedef struct tick {
    float value;
    tick_template* template;
} tick;

tick* new_tick (float value, tick_template* template) {
    tick* t = malloc(sizeof(tick));
    t->value = value;
    t->template = template;
    return t;
}

float value (tick* tick) { return log(tick->value); }
float height (tick* tick) { return tick->template->height; }
char* show (tick* tick) {
    if (tick->template->show) {
        return (tick->template->show)(tick->value);
    } else {
        return NULL;
    }
}

void print_tick (tick* tick) {
    if (tick->template->show) {
        printf("%f %f %s\n", tick->value, tick->template->height, (tick->template->show)(tick->value));
    } else {
        printf("%f %f <>\n", tick->value, tick->template->height);
    }
}

typedef struct subgroup subgroup;

typedef struct group {
    int count;
    tick_template* template;
    int includeInitial;
    int includeFinal;
    subgroup** subs;
} group;

group* new_group (int count, tick_template* template, int includeInitial, int includeFinal, subgroup** subs) {
    group* g = malloc(sizeof(group));
    g->count = count;
    g->template = template;
    g->includeInitial = includeInitial;
    g->includeFinal = includeFinal;
    g->subs = subs;
    return g;
}

group* root (int count, tick_template* template, subgroup** subs) {
    return new_group(count, template, true, true, subs);
}
group* child (int count, tick_template* template, subgroup** subs) {
    return new_group(count, template, false, false, subs);
}

typedef struct subgroup {
    int index;
    group* group;
} subgroup;

subgroup* new_subgroup (int index, group* group) {
    subgroup* sg = malloc(sizeof(subgroup));
    sg->index = index;
    sg->group = group;
    return sg;
}

subgroup** subgroups (int len, ...) {
    int ii = 0;
    subgroup** result = malloc(len * sizeof(subgroup*));

    va_list subs;
    va_start(subs, len);
    while (true) {
        if (ii >= len) break;
        result[ii] = va_arg(subs, subgroup*);
        ii++;
    }

    return result;
}

typedef struct spec {
    float start;
    float end;
    group* group;
} spec;

spec* new_spec (float start, float end, group* group) {
    spec* s = malloc(sizeof(spec));
    s->start = start;
    s->end = end;
    s->group = group;
    return s;
}

typedef struct ticks {
    tick* value;
    struct ticks* next;
} ticks;

tick* node_value (ticks* ts) {
    return ts->value;
}

ticks* node_next (ticks* ts) {
    return ts->next;
}

void append (tick* value, ticks** list) {
    ticks* node = malloc(sizeof(ticks));
    node->value = value;
    node->next = *list;
    *list = node;
}

void split (ticks** ticks, float start, float end, group* g) {
    float delta = (end - start) / (float) g->count;
    float pos = start;
    subgroup** subs = g->subs;
    group* currSubGroup = NULL;

    for (int ii = 0; ii < g->count; ii++) {
        if (subs != NULL && (*subs) != NULL && (*subs)->index == ii) {
            currSubGroup = (*subs)->group;
            subs++;
        }

        if (g->includeInitial || ii > 0) {
            append(new_tick(pos, g->template), ticks);
        }

        if (currSubGroup) {
            split(ticks, pos, pos + delta, currSubGroup);
        }

        pos += delta;
    }

    if (g->includeFinal) {
        append(new_tick(pos, g->template), ticks);
    }
}

ticks* ag1_ticks () {
    group* ag1 = root(
        9, // Number of values
        new_template(1, *prec0),
        // Subgroups
        subgroups(2,
            new_subgroup(
                0,
                child(
                    2, new_template(0.75, *prec1),
                    subgroups(1,
                        new_subgroup(
                            0,
                            child(
                                5, new_template(0.5, NULL),
                                NULL
                            )
                        )
                    )
                )
            ),
            new_subgroup(
                4,
                child(
                    2, new_template(0.75, NULL),
                    subgroups(1,
                        new_subgroup(
                            0,
                            child(
                                2, new_template(0.5, NULL),
                                NULL
                            )
                        )
                    )
                )
            )
        )
    );

    ticks* ticks = NULL;
    split(&ticks, 1, 10, ag1);
    //while (ticks != NULL) {
    //    show_tick(ticks->value);
    //    ticks = ticks->next;
    //}

    return ticks;
}

ticks* cg_ticks () {
    group* cg = root(
        9, // Number of values
        new_template(1, *prec0),
        // Subgroups
        subgroups(3,
            new_subgroup(
                0,
                child(
                    2, new_template(0.75, *prec1),
                    subgroups(1,
                        new_subgroup(
                            0,
                            child(
                                5, new_template(0.5, NULL),
                                subgroups(1,
                                    new_subgroup(
                                        0,
                                        child(
                                            5, new_template(0.25, NULL),
                                            NULL
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            ),
            new_subgroup(
                1,
                child(
                    2, new_template(0.75, NULL),
                    subgroups(1,
                        new_subgroup(
                            0,
                            child(
                                5, new_template(0.5, NULL),
                                subgroups(1,
                                    new_subgroup(
                                        0,
                                        child(
                                            2, new_template(0.25, NULL),
                                            NULL
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            ),
            new_subgroup(
                5,
                child(
                    2, new_template(0.75, NULL),
                    subgroups(1,
                        new_subgroup(
                            0,
                            child(
                                5, new_template(0.5, NULL),
                                NULL
                            )
                        )
                    )
                )
            )
        )
    );

    ticks* ticks = NULL;
    split(&ticks, 1, 10, cg);
    //while (ticks != NULL) {
    //    show_tick(ticks->value);
    //    ticks = ticks->next;
    //}

    return ticks;
}
