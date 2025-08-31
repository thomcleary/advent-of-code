#ifndef DISTANCE_H
#define DISTANCE_H

struct distance
{
    char *from;
    char *to;
    unsigned int distance;
};

struct distance *distance_new(char *line);
void distance_free(struct distance *distance);

#endif