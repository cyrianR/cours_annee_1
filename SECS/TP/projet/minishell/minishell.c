#include <stdio.h>
#include <stdlib.h>
#include "readcmd.h"
#include <stdbool.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>

void traitement(int sig) {
    int status;
    if (sig == SIGCHLD) {
        pid_t pid_fils = waitpid(-1, &status, WNOHANG|WUNTRACED|WCONTINUED);
        if (WIFEXITED(status)) {
            printf("Le processus %d s'est terminé.\n", pid_fils);
        } else if (WIFSIGNALED(status)) {
            printf("Le processus %d a été terminé par un signal.\n", pid_fils);
        } else if (WIFSTOPPED(status)) {
            printf("Le processus %d a été stoppé par un signal.\n", pid_fils);
        } else if (WIFCONTINUED(status)) {
            printf("Le processus %d a été repris par un signal.\n", pid_fils);
        }
    }
}

int main(void) {
    struct sigaction new_action;
    new_action.sa_handler = traitement;
    sigemptyset (&new_action.sa_mask);
    new_action.sa_flags = SA_RESTART;
    sigaction(SIGCHLD, &new_action, NULL);

    bool fini= false;

    while (!fini) {
        printf("> ");
        struct cmdline *commande= readcmd();

        if (commande == NULL) {
            // commande == NULL -> erreur readcmd()
            perror("erreur lecture commande \n");
            exit(EXIT_FAILURE);
    
        } else {

            if (commande->err) {
                // commande->err != NULL -> commande->seq == NULL
                printf("erreur saisie de la commande : %s\n", commande->err);
        
            } else {

                /* Pour le moment le programme ne fait qu'afficher les commandes 
                   tapees et les affiche à l'écran. 
                   Cette partie est à modifier pour considérer l'exécution de ces
                   commandes 
                */
                int indexseq= 0;
                char **cmd;
                while ((cmd= commande->seq[indexseq])) {
                    if (cmd[0]) {
                        if (strcmp(cmd[0], "exit") == 0) {
                            fini= true;
                            printf("Au revoir ...\n");
                        }
                        else {
                            pid_t pid_fils;
							pid_fils = fork();
							switch(pid_fils) {
								case -1:
    							    perror("fork");
	    						    exit(EXIT_FAILURE);
								    break;
								case 0:
									// code fils 
									if (execvp(cmd[0], cmd) == -1) {
								        perror("execvp");
									    exit(EXIT_FAILURE);
									}
									exit(0);
									break;
							}
							if (commande->backgrounded == NULL) {
						        pause();
                            }
                            printf("\n");							
                        }
                        indexseq++;
                    }
                }
            }
        }
    }
    return EXIT_SUCCESS;
}

