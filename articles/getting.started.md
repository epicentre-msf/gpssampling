# Getting started

> If you are not familiar with GPS Sampling here is where to start. Or
> you you want a refresh on how to use GPS Sampling.

## Get access to GPS Sampling

### On your server/computer

GPS Sampling can be deployed **on any server or on a personal
computer**. See the package README for installation instructions.

In any way, the application works better if:

- It runs on a modern browser (i.e.,
  [Firefox](https://www.mozilla.org/firefox/new/),
  [Chrome](https://www.google.com/chrome/),
  [Safari](https://www.apple.com/safari/))
- It runs on a fast and stable internet connection to let a smooth
  rendering of the remote sensing images.

## Overview

When you connect to GPS Sampling, the application appears as in the
image below. Most of the browser window displays a satellite image with
buttons and menus on the sides of the image.

The **Tabs ribbon**, located on the top-left above the satellite image,
displays 4 tabs:

**① Delimit**   **② Identify**   **③ Sample**   **④ Results**  

The 4 tabs allow to execute in sequence the **4 major steps** to
estimate population sizes. Each tab allows to display a new page to
execute specific tasks. A the opening of a new GPS Sampling session, the
page **① Delimit**  is displayed, while the other tabs are temporarily
deactivated. Access to the other pages will be possible when tasks in
the **① Delimit**  page are validated.

The **Info ribbon** located just below the Tabs ribbon, provide basic
information on what is expected to be done in the page displayed.

The **Zooming tools**, located on the top-right side of the image, allow
to zoom in and out. The icon at the very top allows to toggle to the
FullScreen mode.

The **Image tools**, located on the bottom-right side of the image,
allow to select the source and type of satellite image to be displayed.

The **Edit tools**, located on the left side of the image, contains
several buttons and a menu, and it changes depending on the page
displayed.

The view of the page may look slightly different depending on the
resolution of your screen.

![](images/overview.webp)

**GPS Sampling as it appears in a new session with location of the main
functions**

## Pages

### Page **① Delimit**

This page is meant to accomplish the first task which is to **delimit
with one or more polygons the area of interest**. It is possible to
upload a polygon from an external file and/or draw yourself one or more
polygons. Imported or newly created polygons can be edit afterwards.

**Edit tools in this page:**

-  **Validate** the tasks in the page and allow to move to the next
  pages.
-  **Undo** the previous action. Only one undo is possible.
-  **Delete**. This action throw in the bin all polygons imported or
  created. The application is then set to the initial status.
-  **Add a polygon layer** from an external source or file. Follow the
  pop-up window to do so.
-  Activate the **Draw a new polygon** mode. Click on the icon and click
  on the image to start drawing a polygon. At each left-click of the
  mouse a new vertex is created. The polygon is created when the last
  click of the mouse will overlap the first vertex.
-  Activate the **Edit a polygon** mode. Click on the icon and select
  the polygon to be edited. The vertexes of the selected polygons can
  now be moved or deleted. It is also possible to add new vertexes.
  Re-click on the icon to exit from the mode edit.
-  Activate the **Delete polygon** mode. Click on the icon and then
  click on the polygon (or on the polygons one by one) to be deleted.
  Re-click on the icon to exit from the mode delete.
-  **Cut a polygon** allows to split a polygon into two by drawing a
  line throughout the polygon to be split. Make sure that the first and
  last vertex of the line fall outside the polygon to be split.

![](images/page_delimit_with-polygon.png)

**Page Delimit: a polygon delimit the area of interest**

Validate at least one polygon to move th the next pages.

If you opted for the [Roof Simple-Random
sampling](https://yves-amevoin.github.io/gpssampling/articles/) method,
move to Page **② Identify** .

If you opted for the
[Quadrats](https://yves-amevoin.github.io/gpssampling/articles/) or
[T-square](https://yves-amevoin.github.io/gpssampling/articles/) method,
move to Page **③ Sample** .

### Page **② Identify**

Page is to be used only if you opted for the [Roof Simple-Random
sampling](https://yves-amevoin.github.io/gpssampling/articles/) method.

In this page the application has created tiles of 150 metres per side to
cover the polygon(s) saved in the previous page. Tiles are displayed in
white semi-transparent shade.

Tiles are examined one by one, and roofs displayed in the satellite
image are pinpointed with the left-click of the mouse. Erroneous marks
can be deleted by left-clicking on the mark to be deleted.

When all roofs are marked the tiles can be marked as checked or valid .

**Edit tools in this page:** \* **Validate** the tasks in the page and
allow to move to the next pages. \* **Undo** the previous action. Only
one undo is possible. \* **Delete all points**. This action throw in the
bin all points imported or created. The polygon is kept. \* **Import
points for external source**. Follow the pop-up window to do so. \*
**Search and Identify** Chick to the icon to to select and zoom to the
first tile. \* **Checked** Label the tile as “Checked”. Same action can
be done by pressing the \[SPACE\] key on the keyboard. By pressing again
on the \[SPACE\] key, the focus moves on the nearest tile with white
border. \* **Valid** Label the tile as “Valid”. Same action can be done
by pressing the \[ENTER\] key on the keyboard. By pressing again on the
\[ENTER\] key, the focus moves on the nearest tile with yellow border.

![](images/page_identify_white-tiles.png)

**Page Identify: the polygon is covered with square white tiles of 150
metres per side**

### Page **③ Sample**

This page is meant to allow selecting a sample of points within one or
more polygons or from a list of point previously identified, depending
of the sampling method chose.

**Edit tools in this page:**

-  **Validate** the tasks in the page and allow to move to the next
  pages.
-  **Undo** the previous action. Only one undo is possible.
-  **Delete**. This action deletes the selection of the points.
-  **Add a sample layer** from an external source or file. Follow the
  pop-up window to do so.
-  **New Sample**. Follow the pop-up window to select the sampling
  method and the number of points to be sampled.

![](images/page_sample_t-square.png)

**Page Sample: 80 points randomly selected (T-square method)**

Before sampling, you need to select at least one polygon frow which the
points are to be sampled. Do so even if there is only one polygon. For
the [Quadrats](https://yves-amevoin.github.io/gpssampling/articles/)
method you need to chose also the size of the quadrats.

### Page **④ Results**

The page is divided into four main parts:

On the top-left: the **Data entry table**. The table is different
according to the chosen sampling method.

In the centre: the buttons to **Export sampled points in different
formats**. Useful for carrying out the field activity.

On the right: the **Map visualising the sampled points**.

On the bottom-left: the **Results of population estimates**. Results
will be displayed after having entered data in th table an running the
calculation.

![](images/page_results_no-data.png)

**Page Results: Description of the parts in which the page is divided**
