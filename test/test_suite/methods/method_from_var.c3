typedef NodeNotifyHandler = fn void(TreeView* this, TreeNode* node, String prop, void* data);
private fn void TreeView.nodeNotifyHandler(TreeView* this, TreeNode* node, String prop, void* data) {}

struct TreeNode { int abc; NodeNotifyHandler notifyHandler; }
struct TreeView { int abc; }
private fn void TreeView.addNodeInternal(TreeView* this, int nop, TreeNode* node, TreeNode* pnode = null, int pos = -1)
{
	 node.notifyHandler = &(this.nodeNotifyHandler); // #error: Taking the address of a method
}