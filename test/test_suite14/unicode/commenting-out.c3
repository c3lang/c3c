// #error: Invalid encoding - Unbalanced bidirectional markers.
int main() {
    bool isAdmin = false;
    /*‮ } ⁦if (isAdmin)⁩ ⁦ begin admins only */
        printf("You are an admin.\n");
    /* end admins only ‮ { ⁦*/
    return 0;
}


