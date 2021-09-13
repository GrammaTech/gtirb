/*
 * Test file for GTIRB
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void AAA(void) { printf("\nAAA"); }

void AAB(void) { printf("\nAAB"); }

void ABA(void) { printf("\nABA"); }

void ABB(void) { printf("\nABB"); }

void BCA(void) { printf("\nBCA"); }

void BCB(void) { printf("\nBCB"); }

void CCA(void) { printf("\nCCA"); }

void CCB(void) { printf("\nCCB"); }

void AA(int argc) {
  if (argc > 0)
    AAA();
  if (argc > 1)
    AAB();
}

void AB(int argc) {
  if (argc > 0)
    ABA();
  if (argc > 1)
    ABB();
}

void BC(int argc) {
  if (argc > 0)
    BCA();
  if (argc > 1)
    BCB();
}

void CC(int argc) {
  if (argc > 0)
    CCA();
  if (argc > 1)
    CCB();
}

void A(int argc) {
  if (argc > 0)
    AA(argc);
  if (argc > 1)
    AB(argc);
}

void B(int argc) {
  if (argc > 0)
    AB(argc);
  if (argc > 1)
    BC(argc);
}

void C(int argc) {
  if (argc > 0)
    BC(argc);
  if (argc > 1)
    CC(argc);
}

int main(int argc, char* argv[]) {
  if (argc > 0)
    A(argc);
  if (argc > 1)
    B(argc);
  if (argc > 2)
    C(argc);
  printf("\ndone.\n");
  return 0;
}
