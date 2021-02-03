// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

void sema_shadow_error(Decl *decl, Decl *old)
{
	SEMA_ERROR(decl, "'%s' would shadow a previous declaration.", decl->name);
	SEMA_PREV(old, "The previous use of '%s' was here.", decl->name);
}

bool sema_resolve_type_info_maybe_inferred(Context *context, TypeInfo *type_info, bool allow_inferred_type)
{
	if (!sema_resolve_type_shallow(context, type_info, allow_inferred_type)) return false;
	Type *type = type_info->type;
	// usize and similar typedefs will not have a decl.
	if (type->type_kind == TYPE_TYPEDEF && type->decl == NULL) return true;
	if (!type_is_user_defined(type)) return true;
	return sema_analyse_decl(context, type->decl);
}

bool sema_resolve_type_info(Context *context, TypeInfo *type_info)
{
	return sema_resolve_type_info_maybe_inferred(context, type_info, false);
}
