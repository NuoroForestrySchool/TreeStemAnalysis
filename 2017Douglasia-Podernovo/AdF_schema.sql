--
-- File generated with SQLiteStudio v3.1.1 on lun lug 10 09:39:40 2017
--
-- Text encoding used: System
--
PRAGMA foreign_keys = off;
BEGIN TRANSACTION;

-- Table: AdF
DROP TABLE IF EXISTS AdF;

CREATE TABLE AdF (
    CodiceArea    TEXT,
    CodiceRilievo TEXT,
    Id_fusto      TEXT,
    Id_rotella    TEXT,
    raggio        TEXT,
    anno          INT,
    note          TEXT,
    sp_corteccia  DECIMAL,
    misura        DECIMAL,
    PRIMARY KEY (
        CodiceArea,
        CodiceRilievo,
        Id_fusto,
        Id_rotella,
        raggio,
        anno
    ),
    FOREIGN KEY (
        CodiceArea,
        CodiceRilievo,
        Id_fusto,
        Id_rotella
    )
    REFERENCES Rotelle (CodiceArea,
    CodiceRilievo,
    Id_fusto,
    Id_rotella) ON DELETE CASCADE
                ON UPDATE CASCADE
);


-- Table: Aree
DROP TABLE IF EXISTS Aree;

CREATE TABLE Aree (
    CodiceArea     TEXT   PRIMARY KEY,
    Nome           TEXT,
    Sup_m_quadri   DOUBLE NOT NULL,
    Localizzazione TEXT
);


-- Table: Cavallettamento
DROP TABLE IF EXISTS Cavallettamento;

CREATE TABLE Cavallettamento (
    CodiceArea    TEXT,
    CodiceRilievo TEXT,
    d130          INT,
    freq          INT,
    PRIMARY KEY (
        CodiceArea,
        CodiceRilievo,
        d130
    ),
    FOREIGN KEY (
        CodiceArea,
        CodiceRilievo
    )
    REFERENCES Rilievi (CodiceArea,
    CodiceRilievo) ON DELETE CASCADE
                   ON UPDATE CASCADE
);


-- Table: FustiAM
DROP TABLE IF EXISTS FustiAM;

CREATE TABLE FustiAM (
    CodiceArea    TEXT,
    CodiceRilievo TEXT,
    Id_fusto      TEXT,
    d130          INT,
    h_tot         DECIMAL,
    h_tot_note    TEXT,
    PRIMARY KEY (
        CodiceArea,
        CodiceRilievo,
        Id_fusto
    ),
    FOREIGN KEY (
        CodiceArea,
        CodiceRilievo
    )
    REFERENCES Rilievi (CodiceArea,
    CodiceRilievo) 
);


-- Table: IncHtot
DROP TABLE IF EXISTS IncHtot;

CREATE TABLE IncHtot (
    CodiceArea    TEXT,
    CodiceRilievo TEXT,
    Id_fusto      TEXT,
    cimale        TEXT,
    anno          INT,
    IncHtot       TEXT,
    PRIMARY KEY (
        CodiceArea,
        CodiceRilievo,
        Id_fusto,
        cimale,
        anno
    ),
    FOREIGN KEY (
        CodiceArea,
        CodiceRilievo,
        Id_fusto
    )
    REFERENCES FustiAM (CodiceArea,
    CodiceRilievo,
    Id_fusto) ON DELETE CASCADE
              ON UPDATE CASCADE
);


-- Table: Rilievi
DROP TABLE IF EXISTS Rilievi;

CREATE TABLE Rilievi (
    CodiceArea             TEXT REFERENCES Aree (CodiceArea) ON DELETE CASCADE
                                                             ON UPDATE CASCADE,
    CodiceRilievo          TEXT,
    DataRilievo            TEXT,
    Rilevatori             TEXT,
    DescrizioneRilievo     TEXT,
    DescrizionePopolamento TEXT,
    PRIMARY KEY (
        CodiceArea,
        CodiceRilievo
    )
);


-- Table: Rotelle
DROP TABLE IF EXISTS Rotelle;

CREATE TABLE Rotelle (
    CodiceArea    TEXT,
    CodiceRilievo TEXT,
    Id_fusto      TEXT,
    Id_rotella    TEXT,
    h_rotella     DECIMAL,
    note          TEXT,
    PRIMARY KEY (
        CodiceArea,
        CodiceRilievo,
        Id_fusto,
        Id_rotella
    ),
    UNIQUE (
        CodiceArea,
        CodiceRilievo,
        Id_fusto,
        Id_rotella,
        h_rotella
    ),
    FOREIGN KEY (
        CodiceArea,
        CodiceRilievo,
        Id_fusto
    )
    REFERENCES FustiAM (CodiceArea,
    CodiceRilievo,
    Id_fusto) 
);


COMMIT TRANSACTION;
PRAGMA foreign_keys = on;
