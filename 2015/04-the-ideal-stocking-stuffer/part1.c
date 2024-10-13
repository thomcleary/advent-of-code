/*
Day 4: The Ideal Stocking Stuffer (Part 1)
https://adventofcode.com/2015/day/4
*/

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

#define ANSWER 117946

int main(void)
{
    char buffer[BUFSIZ];
    const char key[BUFSIZ] = "ckczppom";
    // const char key[BUFSIZ] = "abcdef"; (Example, answer is 609043)
    bool adventcoin_mined = false;
    unsigned int i = 1;

    // brute force 🐒
    while (!adventcoin_mined)
    {
        char secret[BUFSIZ];
        char suffix[BUFSIZ];

        strcpy(secret, key);
        sprintf(suffix, "%d", i);
        strcat(secret, suffix);

        int pipefd[2]; // pipefd[0] for reading, pipefd[1] for writing

        if (pipe(pipefd) == -1)
        {
            perror("pipe");
            exit(EXIT_FAILURE);
        }

        pid_t pid = fork();
        if (pid == 0)
        {
            close(pipefd[0]);
            dup2(pipefd[1], STDOUT_FILENO);
            close(pipefd[1]);

            execlp("md5", "md5", "-qs", secret, NULL);

            perror("execlp failed");
            exit(EXIT_FAILURE);
        }
        else
        {
            close(pipefd[1]);
            read(pipefd[0], buffer, sizeof(buffer) - 1);
            close(pipefd[0]);

            wait(NULL);

            if (strncmp(buffer, "00000", 5) == 0)
            {
                break;
            }

            i++;
        }
    }

    printf("Answer: %d\n", i);

    assert(i == ANSWER);

    return 0;
}