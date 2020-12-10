/*
 * Clip.cpp - implementation of class clip which holds notes
 *
 * Copyright (c) 2004-2014 Tobias Doerffel <tobydox/at/users.sourceforge.net>
 * Copyright (c) 2005-2007 Danny McRae <khjklujn/at/yahoo.com>
 *
 * This file is part of LMMS - https://lmms.io
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program (see COPYING); if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA.
 *
 */
#include "Clip.h"

#include <QApplication>
#include <QTimer>
#include <QMenu>
#include <QMouseEvent>
#include <QPainter>
#include <QPushButton>
#include <QTimer>

#include "AudioSampleRecorder.h"
#include "BBTrackContainer.h"
#include "DeprecationHelper.h"
#include "embed.h"
#include "gui_templates.h"
#include "GuiApplication.h"
#include "InstrumentTrack.h"
#include "PianoRoll.h"
#include "RenameDialog.h"

#include <limits>


QPixmap * ClipView::s_stepBtnOn0 = NULL;
QPixmap * ClipView::s_stepBtnOn200 = NULL;
QPixmap * ClipView::s_stepBtnOff = NULL;
QPixmap * ClipView::s_stepBtnOffLight = NULL;



Clip::Clip( InstrumentTrack * _instrument_track ) :
	TrackContentObject( _instrument_track ),
	m_instrumentTrack( _instrument_track ),
	m_clipType( BeatClip ),
	m_steps( TimePos::stepsPerBar() )
{
	if( _instrument_track->trackContainer()
					== Engine::getBBTrackContainer() )
	{
		resizeToFirstTrack();
	}
	init();
	setAutoResize( true );
}




Clip::Clip( const Clip& other ) :
	TrackContentObject( other.m_instrumentTrack ),
	m_instrumentTrack( other.m_instrumentTrack ),
	m_clipType( other.m_clipType ),
	m_steps( other.m_steps )
{
	for( NoteVector::ConstIterator it = other.m_notes.begin(); it != other.m_notes.end(); ++it )
	{
		m_notes.push_back( new Note( **it ) );
	}

	init();
	switch( getTrack()->trackContainer()->type() )
	{
		case TrackContainer::BBContainer:
			setAutoResize( true );
			break;

		case TrackContainer::SongContainer:
			// move down
		default:
			setAutoResize( false );
			break;
	}
}


Clip::~Clip()
{
	emit destroyedClip( this );

	for( NoteVector::Iterator it = m_notes.begin();
						it != m_notes.end(); ++it )
	{
		delete *it;
	}

	m_notes.clear();
}




void Clip::resizeToFirstTrack()
{
	// Resize this track to be the same as existing tracks in the BB
	const TrackContainer::TrackList & tracks =
		m_instrumentTrack->trackContainer()->tracks();
	for(unsigned int trackID = 0; trackID < tracks.size(); ++trackID)
	{
		if(tracks.at(trackID)->type() == Track::InstrumentTrack)
		{
			if(tracks.at(trackID) != m_instrumentTrack)
			{
				unsigned int currentTCO = m_instrumentTrack->
					getTCOs().indexOf(this);
				m_steps = static_cast<Clip *>
					(tracks.at(trackID)->getTCO(currentTCO))
					->m_steps;
			}
			break;
		}
	}
}




void Clip::init()
{
	connect( Engine::getSong(), SIGNAL( timeSignatureChanged( int, int ) ),
				this, SLOT( changeTimeSignature() ) );
	saveJournallingState( false );

	updateLength();
	restoreJournallingState();
}




void Clip::updateLength()
{
	if( m_clipType == BeatClip )
	{
		changeLength( beatClipLength() );
		updateBBTrack();
		return;
	}

	tick_t max_length = TimePos::ticksPerBar();

	for( NoteVector::ConstIterator it = m_notes.begin();
						it != m_notes.end(); ++it )
	{
		if( ( *it )->length() > 0 )
		{
			max_length = qMax<tick_t>( max_length,
							( *it )->endPos() );
		}
	}
	changeLength( TimePos( max_length ).nextFullBar() *
						TimePos::ticksPerBar() );
	updateBBTrack();
}




TimePos Clip::beatClipLength() const
{
	tick_t max_length = TimePos::ticksPerBar();

	for( NoteVector::ConstIterator it = m_notes.begin();
						it != m_notes.end(); ++it )
	{
		if( ( *it )->length() < 0 )
		{
			max_length = qMax<tick_t>( max_length,
				( *it )->pos() + 1 );
		}
	}

	if( m_steps != TimePos::stepsPerBar() )
	{
		max_length = m_steps * TimePos::ticksPerBar() /
						TimePos::stepsPerBar();
	}

	return TimePos( max_length ).nextFullBar() * TimePos::ticksPerBar();
}




Note * Clip::addNote( const Note & _new_note, const bool _quant_pos )
{
	Note * new_note = new Note( _new_note );
	if( _quant_pos && gui->pianoRoll() )
	{
		new_note->quantizePos( gui->pianoRoll()->quantization() );
	}

	instrumentTrack()->lock();
	m_notes.insert(std::upper_bound(m_notes.begin(), m_notes.end(), new_note, Note::lessThan), new_note);
	instrumentTrack()->unlock();

	checkType();
	updateLength();

	emit dataChanged();

	return new_note;
}




void Clip::removeNote( Note * _note_to_del )
{
	instrumentTrack()->lock();
	NoteVector::Iterator it = m_notes.begin();
	while( it != m_notes.end() )
	{
		if( *it == _note_to_del )
		{
			delete *it;
			m_notes.erase( it );
			break;
		}
		++it;
	}
	instrumentTrack()->unlock();

	checkType();
	updateLength();

	emit dataChanged();
}


// returns a pointer to the note at specified step, or NULL if note doesn't exist

Note * Clip::noteAtStep( int _step )
{
	for( NoteVector::Iterator it = m_notes.begin(); it != m_notes.end();
									++it )
	{
		if( ( *it )->pos() == TimePos::stepPosition( _step )
						&& ( *it )->length() < 0 )
		{
			return *it;
		}
	}
	return NULL;
}



void Clip::rearrangeAllNotes()
{
	// sort notes by start time
	std::sort(m_notes.begin(), m_notes.end(), Note::lessThan);
}



void Clip::clearNotes()
{
	instrumentTrack()->lock();
	for( NoteVector::Iterator it = m_notes.begin(); it != m_notes.end();
									++it )
	{
		delete *it;
	}
	m_notes.clear();
	instrumentTrack()->unlock();

	checkType();
	emit dataChanged();
}




Note * Clip::addStepNote( int step )
{
	return addNote( Note( TimePos( -DefaultTicksPerBar ),
				TimePos::stepPosition( step ) ), false );
}




void Clip::setStep( int step, bool enabled )
{
	if( enabled )
	{
		if ( !noteAtStep( step ) )
		{
			addStepNote( step );
		}
		return;
	}

	while( Note * note = noteAtStep( step ) )
	{
		removeNote( note );
	}
}




void Clip::setType( ClipTypes new_clip_type )
{
	if( new_clip_type == BeatClip ||
				new_clip_type == MelodyClip )
	{
		m_clipType = new_clip_type;
	}
}




void Clip::checkType()
{
	NoteVector::Iterator it = m_notes.begin();
	while( it != m_notes.end() )
	{
		if( ( *it )->length() > 0 )
		{
			setType( MelodyClip );
			return;
		}
		++it;
	}
	setType( BeatClip );
}




void Clip::saveSettings( QDomDocument & _doc, QDomElement & _this )
{
	_this.setAttribute( "type", m_clipType );
	_this.setAttribute( "name", name() );
	
	if( usesCustomClipColor() )
	{
		_this.setAttribute( "color", color().name() );
	}
	// as the target of copied/dragged clip is always an existing
	// clip, we must not store actual position, instead we store -1
	// which tells loadSettings() not to mess around with position
	if( _this.parentNode().nodeName() == "clipboard" ||
			_this.parentNode().nodeName() == "dnddata" )
	{
		_this.setAttribute( "pos", -1 );
	}
	else
	{
		_this.setAttribute( "pos", startPosition() );
	}
	_this.setAttribute( "muted", isMuted() );
	_this.setAttribute( "steps", m_steps );

	// now save settings of all notes
	for( NoteVector::Iterator it = m_notes.begin();
						it != m_notes.end(); ++it )
	{
		( *it )->saveState( _doc, _this );
	}
}




void Clip::loadSettings( const QDomElement & _this )
{
	m_clipType = static_cast<ClipTypes>( _this.attribute( "type"
								).toInt() );
	setName( _this.attribute( "name" ) );
	
	if( _this.hasAttribute( "color" ) )
	{
		useCustomClipColor( true );
		setColor( _this.attribute( "color" ) );
	}
	
	if( _this.attribute( "pos" ).toInt() >= 0 )
	{
		movePosition( _this.attribute( "pos" ).toInt() );
	}
	if( _this.attribute( "muted" ).toInt() != isMuted() )
	{
		toggleMute();
	}

	clearNotes();

	QDomNode node = _this.firstChild();
	while( !node.isNull() )
	{
		if( node.isElement() &&
			!node.toElement().attribute( "metadata" ).toInt() )
		{
			Note * n = new Note;
			n->restoreState( node.toElement() );
			m_notes.push_back( n );
		}
		node = node.nextSibling();
        }

	m_steps = _this.attribute( "steps" ).toInt();
	if( m_steps == 0 )
	{
		m_steps = TimePos::stepsPerBar();
	}

	checkType();
	updateLength();

	emit dataChanged();
}




Clip *  Clip::previousClip() const
{
	return adjacentClipByOffset(-1);
}




Clip *  Clip::nextClip() const
{
	return adjacentClipByOffset(1);
}




Clip * Clip::adjacentClipByOffset(int offset) const
{
	QVector<TrackContentObject *> tcos = m_instrumentTrack->getTCOs();
	int tcoNum = m_instrumentTrack->getTCONum(this);
	return dynamic_cast<Clip*>(tcos.value(tcoNum + offset, NULL));
}




void Clip::clear()
{
	addJournalCheckPoint();
	clearNotes();
}




void Clip::addSteps()
{
	m_steps += TimePos::stepsPerBar();
	updateLength();
	emit dataChanged();
}

void Clip::cloneSteps()
{
	int oldLength = m_steps;
	m_steps *= 2; // cloning doubles the track
	for(int i = 0; i < oldLength; ++i )
	{
		Note *toCopy = noteAtStep( i );
		if( toCopy )
		{
			setStep( oldLength + i, true );
			Note *newNote = noteAtStep( oldLength + i );
			newNote->setKey( toCopy->key() );
			newNote->setLength( toCopy->length() );
			newNote->setPanning( toCopy->getPanning() );
			newNote->setVolume( toCopy->getVolume() );
		}
	}
	updateLength();
	emit dataChanged();
}




void Clip::removeSteps()
{
	int n = TimePos::stepsPerBar();
	if( n < m_steps )
	{
		for( int i = m_steps - n; i < m_steps; ++i )
		{
			setStep( i, false );
		}
		m_steps -= n;
		updateLength();
		emit dataChanged();
	}
}




TrackContentObjectView * Clip::createView( TrackView * _tv )
{
	return new ClipView( this, _tv );
}




void Clip::updateBBTrack()
{
	if( getTrack()->trackContainer() == Engine::getBBTrackContainer() )
	{
		Engine::getBBTrackContainer()->updateBBTrack( this );
	}

	if( gui && gui->pianoRoll() && gui->pianoRoll()->currentClip() == this )
	{
		gui->pianoRoll()->update();
	}
}




bool Clip::empty()
{
	for( NoteVector::ConstIterator it = m_notes.begin();
						it != m_notes.end(); ++it )
	{
		if( ( *it )->length() != 0 )
		{
			return false;
		}
	}
	return true;
}




void Clip::changeTimeSignature()
{
	TimePos last_pos = TimePos::ticksPerBar() - 1;
	for( NoteVector::ConstIterator cit = m_notes.begin();
						cit != m_notes.end(); ++cit )
	{
		if( ( *cit )->length() < 0 && ( *cit )->pos() > last_pos )
		{
			last_pos = ( *cit )->pos()+TimePos::ticksPerBar() /
						TimePos::stepsPerBar();
		}
	}
	last_pos = last_pos.nextFullBar() * TimePos::ticksPerBar();
	m_steps = qMax<tick_t>( TimePos::stepsPerBar(),
				last_pos.getBar() * TimePos::stepsPerBar() );
	updateLength();
}





ClipView::ClipView( Clip* clip, TrackView* parent ) :
	TrackContentObjectView( clip, parent ),
	m_clip( clip ),
	m_paintPixmap(),
	m_noteFillColor(255, 255, 255, 220),
	m_noteBorderColor(255, 255, 255, 220),
	m_mutedNoteFillColor(100, 100, 100, 220),
	m_mutedNoteBorderColor(100, 100, 100, 220)
{
	connect( gui->pianoRoll(), SIGNAL( currentClipChanged() ),
			this, SLOT( update() ) );

	if( s_stepBtnOn0 == NULL )
	{
		s_stepBtnOn0 = new QPixmap( embed::getIconPixmap(
							"step_btn_on_0" ) );
	}

	if( s_stepBtnOn200 == NULL )
	{
		s_stepBtnOn200 = new QPixmap( embed::getIconPixmap(
							"step_btn_on_200" ) );
	}

	if( s_stepBtnOff == NULL )
	{
		s_stepBtnOff = new QPixmap( embed::getIconPixmap(
							"step_btn_off" ) );
	}

	if( s_stepBtnOffLight == NULL )
	{
		s_stepBtnOffLight = new QPixmap( embed::getIconPixmap(
						"step_btn_off_light" ) );
	}

	update();

	setStyle( QApplication::style() );
}

void ClipView::update()
{
	ToolTip::add(this, m_clip->name());

	TrackContentObjectView::update();
}




void ClipView::openInPianoRoll()
{
	gui->pianoRoll()->setCurrentClip( m_clip );
	gui->pianoRoll()->parentWidget()->show();
	gui->pianoRoll()->show();
	gui->pianoRoll()->setFocus();
}





void ClipView::setGhostInPianoRoll()
{
	gui->pianoRoll()->setGhostClip( m_clip );
	gui->pianoRoll()->parentWidget()->show();
	gui->pianoRoll()->show();
	gui->pianoRoll()->setFocus();
}




void ClipView::resetName() { m_clip->setName(""); }




void ClipView::changeName()
{
	QString s = m_clip->name();
	RenameDialog rename_dlg( s );
	rename_dlg.exec();
	m_clip->setName( s );
}




void ClipView::constructContextMenu( QMenu * _cm )
{
	QAction * a = new QAction( embed::getIconPixmap( "piano" ),
					tr( "Open in piano-roll" ), _cm );
	_cm->insertAction( _cm->actions()[0], a );
	connect( a, SIGNAL( triggered( bool ) ),
					this, SLOT( openInPianoRoll() ) );

	QAction * b = new QAction( embed::getIconPixmap( "ghost_note" ),
						tr( "Set as ghost in piano-roll" ), _cm );
	if( m_clip->empty() ) { b->setEnabled( false ); }
	_cm->insertAction( _cm->actions()[1], b );
	connect( b, SIGNAL( triggered( bool ) ),
					this, SLOT( setGhostInPianoRoll() ) );
	_cm->insertSeparator( _cm->actions()[2] );
	_cm->addSeparator();

	_cm->addAction( embed::getIconPixmap( "edit_erase" ),
			tr( "Clear all notes" ), m_clip, SLOT( clear() ) );
	_cm->addSeparator();

	_cm->addAction( embed::getIconPixmap( "reload" ), tr( "Reset name" ),
						this, SLOT( resetName() ) );
	_cm->addAction( embed::getIconPixmap( "edit_rename" ),
						tr( "Change name" ),
						this, SLOT( changeName() ) );

	if ( m_clip->type() == Clip::BeatClip )
	{
		_cm->addSeparator();

		_cm->addAction( embed::getIconPixmap( "step_btn_add" ),
			tr( "Add steps" ), m_clip, SLOT( addSteps() ) );
		_cm->addAction( embed::getIconPixmap( "step_btn_remove" ),
			tr( "Remove steps" ), m_clip, SLOT( removeSteps() ) );
		_cm->addAction( embed::getIconPixmap( "step_btn_duplicate" ),
			tr( "Clone Steps" ), m_clip, SLOT( cloneSteps() ) );
	}
}




void ClipView::mousePressEvent( QMouseEvent * _me )
{
	if( _me->button() == Qt::LeftButton &&
				m_clip->m_clipType == Clip::BeatClip &&
				( fixedTCOs() || pixelsPerBar() >= 96 ) &&
				_me->y() > height() - s_stepBtnOff->height() )

	// when mouse button is pressed in beat/bassline -mode

	{
//	get the step number that was clicked on and
//	do calculations in floats to prevent rounding errors...
		float tmp = ( ( float(_me->x()) - TCO_BORDER_WIDTH ) *
				float( m_clip -> m_steps ) ) / float(width() - TCO_BORDER_WIDTH*2);

		int step = int( tmp );

//	debugging to ensure we get the correct step...
//		qDebug( "Step (%f) %d", tmp, step );

		if( step >= m_clip->m_steps )
		{
			qDebug( "Something went wrong in clip.cpp: step %d doesn't exist in clip!", step );
			return;
		}

		Note * n = m_clip->noteAtStep( step );

		if( n == NULL )
		{
			m_clip->addStepNote( step );
		}
		else // note at step found
		{
			m_clip->addJournalCheckPoint();
			m_clip->setStep( step, false );
		}

		Engine::getSong()->setModified();
		update();

		if( gui->pianoRoll()->currentClip() == m_clip )
		{
			gui->pianoRoll()->update();
		}
	}
	else

	// if not in beat/bassline -mode, let parent class handle the event

	{
		TrackContentObjectView::mousePressEvent( _me );
	}
}

void ClipView::mouseDoubleClickEvent(QMouseEvent *_me)
{
	if( _me->button() != Qt::LeftButton )
	{
		_me->ignore();
		return;
	}
	if( m_clip->m_clipType == Clip::MelodyClip || !fixedTCOs() )
	{
		openInPianoRoll();
	}
}




void ClipView::wheelEvent(QWheelEvent * we)
{
	if(m_clip->m_clipType == Clip::BeatClip &&
				(fixedTCOs() || pixelsPerBar() >= 96) &&
				position(we).y() > height() - s_stepBtnOff->height())
	{
//	get the step number that was wheeled on and
//	do calculations in floats to prevent rounding errors...
		float tmp = ((float(position(we).x()) - TCO_BORDER_WIDTH) *
				float(m_clip -> m_steps)) / float(width() - TCO_BORDER_WIDTH*2);

		int step = int( tmp );

		if( step >= m_clip->m_steps )
		{
			return;
		}

		Note * n = m_clip->noteAtStep( step );
		if(!n && we->angleDelta().y() > 0)
		{
			n = m_clip->addStepNote( step );
			n->setVolume( 0 );
		}
		if( n != NULL )
		{
			int vol = n->getVolume();

			if(we->angleDelta().y() > 0)
			{
				n->setVolume( qMin( 100, vol + 5 ) );
			}
			else
			{
				n->setVolume( qMax( 0, vol - 5 ) );
			}

			Engine::getSong()->setModified();
			update();
			if( gui->pianoRoll()->currentClip() == m_clip )
			{
				gui->pianoRoll()->update();
			}
		}
		we->accept();
	}
	else
	{
		TrackContentObjectView::wheelEvent(we);
	}
}


static int computeNoteRange(int minKey, int maxKey)
{
	return (maxKey - minKey) + 1;
}

void ClipView::paintEvent( QPaintEvent * )
{
	QPainter painter( this );

	if( !needsUpdate() )
	{
		painter.drawPixmap( 0, 0, m_paintPixmap );
		return;
	}

	setNeedsUpdate( false );

	if (m_paintPixmap.isNull() || m_paintPixmap.size() != size())
	{
		m_paintPixmap = QPixmap(size());
	}

	QPainter p( &m_paintPixmap );

	QColor c;
	bool const muted = m_clip->getTrack()->isMuted() || m_clip->isMuted();
	bool current = gui->pianoRoll()->currentClip() == m_clip;
	bool beatClip = m_clip->m_clipType == Clip::BeatClip;
	
	if( beatClip )
	{
		// Do not paint BBTCOs how we paint clip TCOs
		c = BBPatternBackground();
	}
	else
	{
		c = getColorForDisplay( painter.background().color() );
	}

	// invert the gradient for the background in the B&B editor
	QLinearGradient lingrad( 0, 0, 0, height() );
	lingrad.setColorAt( beatClip ? 0 : 1, c.darker( 300 ) );
	lingrad.setColorAt( beatClip ? 1 : 0, c );

	// paint a black rectangle under the clip to prevent glitches with transparent backgrounds
	p.fillRect( rect(), QColor( 0, 0, 0 ) );

	if( gradient() )
	{
		p.fillRect( rect(), lingrad );
	}
	else
	{
		p.fillRect( rect(), c );
	}

	// Check whether we will paint a text box and compute its potential height
	// This is needed so we can paint the notes underneath it.
	bool const drawName = !m_clip->name().isEmpty();
	bool const drawTextBox = !beatClip && drawName;

	// TODO Warning! This might cause problems if TrackContentObjectView::paintTextLabel changes
	int textBoxHeight = 0;
	const int textTop = TCO_BORDER_WIDTH + 1;
	if (drawTextBox)
	{
		QFont labelFont = this->font();
		labelFont.setHintingPreference( QFont::PreferFullHinting );

		QFontMetrics fontMetrics(labelFont);
		textBoxHeight = fontMetrics.height() + 2 * textTop;
	}

	// Compute pixels per bar
	const int baseWidth = fixedTCOs() ? parentWidget()->width() - 2 * TCO_BORDER_WIDTH
						: width() - TCO_BORDER_WIDTH;
	const float pixelsPerBar = baseWidth / (float) m_clip->length().getBar();

	// Length of one bar/beat in the [0,1] x [0,1] coordinate system
	const float barLength = 1. / m_clip->length().getBar();
	const float tickLength = barLength / TimePos::ticksPerBar();

	const int x_base = TCO_BORDER_WIDTH;

	// melody clip paint event
	NoteVector const & noteCollection = m_clip->m_notes;
	if( m_clip->m_clipType == Clip::MelodyClip && !noteCollection.empty() )
	{
		// Compute the minimum and maximum key in the clip
		// so that we know how much there is to draw.
		int maxKey = std::numeric_limits<int>::min();
		int minKey = std::numeric_limits<int>::max();

		for (Note const * note : noteCollection)
		{
			int const key = note->key();
			maxKey = qMax( maxKey, key );
			minKey = qMin( minKey, key );
		}

		// If needed adjust the note range so that we always have paint a certain interval
		int const minimalNoteRange = 12; // Always paint at least one octave
		int const actualNoteRange = computeNoteRange(minKey, maxKey);

		if (actualNoteRange < minimalNoteRange)
		{
			int missingNumberOfNotes = minimalNoteRange - actualNoteRange;
			minKey = std::max(0, minKey - missingNumberOfNotes / 2);
			maxKey = maxKey + missingNumberOfNotes / 2;
			if (missingNumberOfNotes % 2 == 1)
			{
				// Put more range at the top to bias drawing towards the bottom
				++maxKey;
			}
		}

		int const adjustedNoteRange = computeNoteRange(minKey, maxKey);

		// Transform such that [0, 1] x [0, 1] paints in the correct area
		float distanceToTop = textBoxHeight;

		// This moves the notes smoothly under the text
		int widgetHeight = height();
		int fullyAtTopAtLimit = MINIMAL_TRACK_HEIGHT;
		int fullyBelowAtLimit = 4 * fullyAtTopAtLimit;
		if (widgetHeight <= fullyBelowAtLimit)
		{
			if (widgetHeight <= fullyAtTopAtLimit)
			{
				distanceToTop = 0;
			}
			else
			{
				float const a = 1. / (fullyAtTopAtLimit - fullyBelowAtLimit);
				float const b = - float(fullyBelowAtLimit) / (fullyAtTopAtLimit - fullyBelowAtLimit);
				float const scale = a * widgetHeight + b;
				distanceToTop = (1. - scale) * textBoxHeight;
			}
		}

		int const notesBorder = 4; // Border for the notes towards the top and bottom in pixels

		// The relavant painting code starts here
		p.save();

		p.translate(0., distanceToTop + notesBorder);
		p.scale(width(), height() - distanceToTop - 2 * notesBorder);

		// set colour based on mute status
		QColor noteFillColor = muted ? getMutedNoteFillColor() : getNoteFillColor();
		QColor noteBorderColor = muted ? getMutedNoteBorderColor()
									   : ( m_clip->hasColor() ? c.lighter( 200 ) : getNoteBorderColor() );

		bool const drawAsLines = height() < 64;
		if (drawAsLines)
		{
			p.setPen(noteFillColor);
		}
		else
		{
			p.setPen(noteBorderColor);
			p.setRenderHint(QPainter::Antialiasing);
		}

		// Needed for Qt5 although the documentation for QPainter::setPen(QColor) as it's used above
		// states that it should already set a width of 0.
		QPen pen = p.pen();
		pen.setWidth(0);
		p.setPen(pen);

		float const noteHeight = 1. / adjustedNoteRange;

		// scan through all the notes and draw them on the clip
		for (Note const * currentNote : noteCollection)
		{
			// Map to 0, 1, 2, ...
			int mappedNoteKey = currentNote->key() - minKey;
			int invertedMappedNoteKey = adjustedNoteRange - mappedNoteKey - 1;

			float const noteStartX = currentNote->pos() * tickLength;
			float const noteLength = currentNote->length() * tickLength;

			float const noteStartY = invertedMappedNoteKey * noteHeight;

			QRectF noteRectF( noteStartX, noteStartY, noteLength, noteHeight);
			if (drawAsLines)
			{
				p.drawLine(QPointF(noteStartX, noteStartY + 0.5 * noteHeight),
					   QPointF(noteStartX + noteLength, noteStartY + 0.5 * noteHeight));
			}
			else
			{
				p.fillRect( noteRectF, noteFillColor );
				p.drawRect( noteRectF );
			}
		}

		p.restore();
	}

	// beat clip paint event
	else if( beatClip &&	( fixedTCOs() || pixelsPerBar >= 96 ) )
	{
		QPixmap stepon0;
		QPixmap stepon200;
		QPixmap stepoff;
		QPixmap stepoffl;
		const int steps = qMax( 1,
					m_clip->m_steps );
		const int w = width() - 2 * TCO_BORDER_WIDTH;

		// scale step graphics to fit the beat clip length
		stepon0 = s_stepBtnOn0->scaled( w / steps,
					      s_stepBtnOn0->height(),
					      Qt::IgnoreAspectRatio,
					      Qt::SmoothTransformation );
		stepon200 = s_stepBtnOn200->scaled( w / steps,
					      s_stepBtnOn200->height(),
					      Qt::IgnoreAspectRatio,
					      Qt::SmoothTransformation );
		stepoff = s_stepBtnOff->scaled( w / steps,
						s_stepBtnOff->height(),
						Qt::IgnoreAspectRatio,
						Qt::SmoothTransformation );
		stepoffl = s_stepBtnOffLight->scaled( w / steps,
						s_stepBtnOffLight->height(),
						Qt::IgnoreAspectRatio,
						Qt::SmoothTransformation );

		for( int it = 0; it < steps; it++ )	// go through all the steps in the beat clip
		{
			Note * n = m_clip->noteAtStep( it );

			// figure out x and y coordinates for step graphic
			const int x = TCO_BORDER_WIDTH + static_cast<int>( it * w / steps );
			const int y = height() - s_stepBtnOff->height() - 1;

			if( n )
			{
				const int vol = n->getVolume();
				p.drawPixmap( x, y, stepoffl );
				p.drawPixmap( x, y, stepon0 );
				p.setOpacity( sqrt( vol / 200.0 ) );
				p.drawPixmap( x, y, stepon200 );
				p.setOpacity( 1 );
			}
			else if( ( it / 4 ) % 2 )
			{
				p.drawPixmap( x, y, stepoffl );
			}
			else
			{
				p.drawPixmap( x, y, stepoff );
			}
		} // end for loop

		// draw a transparent rectangle over muted clips
		if ( muted )
		{
			p.setBrush( mutedBackgroundColor() );
			p.setOpacity( 0.5 );
			p.drawRect( 0, 0, width(), height() );
		}
	}

	// bar lines
	const int lineSize = 3;
	p.setPen( c.darker( 200 ) );

	for( bar_t t = 1; t < m_clip->length().getBar(); ++t )
	{
		p.drawLine( x_base + static_cast<int>( pixelsPerBar * t ) - 1,
				TCO_BORDER_WIDTH, x_base + static_cast<int>(
						pixelsPerBar * t ) - 1, TCO_BORDER_WIDTH + lineSize );
		p.drawLine( x_base + static_cast<int>( pixelsPerBar * t ) - 1,
				rect().bottom() - ( lineSize + TCO_BORDER_WIDTH ),
				x_base + static_cast<int>( pixelsPerBar * t ) - 1,
				rect().bottom() - TCO_BORDER_WIDTH );
	}

	// clip name
	if (drawTextBox)
	{
		paintTextLabel(m_clip->name(), p);
	}

	if( !( fixedTCOs() && beatClip ) )
	{
		// inner border
		p.setPen( c.lighter( current ? 160 : 130 ) );
		p.drawRect( 1, 1, rect().right() - TCO_BORDER_WIDTH,
			rect().bottom() - TCO_BORDER_WIDTH );

		// outer border
		p.setPen( current ? c.lighter( 130 ) : c.darker( 300 ) );
		p.drawRect( 0, 0, rect().right(), rect().bottom() );
	}

	// draw the 'muted' pixmap only if the clip was manually muted
	if( m_clip->isMuted() )
	{
		const int spacing = TCO_BORDER_WIDTH;
		const int size = 14;
		p.drawPixmap( spacing, height() - ( size + spacing ),
			embed::getIconPixmap( "muted", size, size ) );
	}

	painter.drawPixmap( 0, 0, m_paintPixmap );
}
