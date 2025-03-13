alias NodeNotifyHandler = fn void(TreeView* this, TreeNode* node, String prop, void* data);
fn void TreeView.nodeNotifyHandler(TreeView* this, TreeNode* node, String prop, void* data) @private {}

struct TreeNode { int abc; NodeNotifyHandler notifyHandler; }
struct TreeView { int abc; }
fn void TreeView.addNodeInternal(TreeView* this, int nop, TreeNode* node, TreeNode* pnode = null, int pos = -1) @private
{
        node.notifyHandler = &TreeView.nodeNotifyHandler; // This is the line
}