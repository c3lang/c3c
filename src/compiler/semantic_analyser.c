// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

void sema_shadow_error(Decl *decl, Decl *old)
{
	SEMA_ERROR(decl, "The '%s' would shadow a previous declaration.", decl->name);
	SEMA_PREV(old, "The previous use of '%s' was here.", decl->name);
}


bool sema_resolve_type_info(Context *context, TypeInfo *type_info)
{
	if (!sema_resolve_type_shallow(context, type_info)) return false;
	return true;
}
