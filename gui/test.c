#include <AL/alut.h>
#include <GL/glut.h>

#include "main.h"

int main(int argc, char *argv[])
{
	gDisplay disp;

	disp.vp_h = 480;
	disp.vp_w = 640;
	glutInit(&argc, argv);
	loadSettings("configs/player.cfg");
	loadSettings("configs/graphics.cfg");
	loadSettings("configs/sound.cfg");
	parseSettings();
	setupSound(&argc, argv);
	setupDisplay(&disp);
	switchCallbacks(&guiCallbacks);
	switchCallbacks(&guiCallbacks);
	glutMainLoop();
}
