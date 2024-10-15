#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "distance.h"

struct distance *distance_new(char *line)
{
    struct distance *distance = malloc(sizeof(struct distance));
    assert(distance != NULL);

    char *line_copy = strdup(line);
    assert(line_copy != NULL);
    char *line_copy_to_free = line_copy;

    char *locations = strsep(&line_copy, "=");

    unsigned int dist;
    sscanf(line_copy, " %d", &dist);

    char *from = strdup(strsep(&locations, " "));

    char *to = strdup(locations + 3); // skip over "to "
    to[strcspn(to, " ")] = '\0';      // trim the trailing space

    distance->from = from;
    distance->to = to;
    distance->distance = dist;

    free(line_copy_to_free);

    return distance;
}

void distance_free(struct distance *distance)
{
    free(distance->from);
    free(distance->to);
    free(distance);
}