import gtk;

const string CLICK_ME = "Click Me";
uint counter = 0;

fn void clickedme(GtkButton *o, void *d)
{
    (GtkLabel*)(d).set_text(string.format("You clicked me %d times", ++counter));
}

int main(int argc as string[] argv)
{
    gtk::init(&argc, &argv);

    GtkWindow *win = gtk::windowCreate(GtkWindow::TOPLEVEL);
    win.set_title(CLICK_ME);

    GtkButton *button = gtk::buttonCreateWithLabel(CLICK_ME);

    GtkLabel *label = GtkLabel.new("There have been no clicks yet");
    label.setSingleLineMode(true);

    GtkVBox vbox = gtk::vBoxCreate(true, 1);
    vbox.add(label);
    vbox.add(button);

    win.add(vbox);

    win.connectSignal("delete-event", gtk::mainQuit, NULL);
    button.connectSignal("clicked", &clickedme, label);

    win.showAll();
    gtk::main();
    return 0;
}