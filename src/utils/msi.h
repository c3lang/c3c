#ifndef C3_MSI_H
#define C3_MSI_H

#include <stdbool.h>

/**
 * Extracts files from an MSI package.
 * @param msi_path Path to the .msi file.
 * @param out_root Root directory to extract to.
 * @param cab_dir Directory where external .cab files are located.
 * @return true on success, false on failure.
 */
bool msi_extract(const char *msi_path, const char *out_root, const char *cab_dir, bool verbose);

#endif // C3_MSI_H
