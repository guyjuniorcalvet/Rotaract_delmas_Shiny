-- Création de la base de données
# DROP DATABASE Rotaract_de_Delmas;
CREATE DATABASE IF NOT EXISTS Rotaract_de_Delmas;
USE Rotaract_de_Delmas;

-- Table : Membres
CREATE TABLE Membres (
    ID_membre INT AUTO_INCREMENT PRIMARY KEY,
    Nom VARCHAR(50) NOT NULL,
    Prenom VARCHAR(50) NOT NULL,
    Genre ENUM('Homme', 'Femme') NOT NULL,
    Date_naissance DATE NOT NULL,
    Poste ENUM('Président(e)', 'Past-Président(e)', 'Vice-président(e)', 'Secrétaire', 'Secrétaire-adjoint', 'Trésorier(e)',
    'Membre', 'Responsable-pro', 'Responsable-int', 'Responsable-comu', 'Responsable-in', 'Invité(e)', 'Ami(e)') NOT NULL,
    Statut ENUM('Actif', 'Non-Actif') NOT NULL,
    Date_debut DATE NOT NULL,
    Profession ENUM('Etudiant(e)', 'Professionnel(le)-actif', 'Etudiant-Professionnel(le)') NOT NULL,
    Ville VARCHAR(25) NOT NULL,
    Etat VARCHAR(25),
    Pays VARCHAR(25) NOT NULL,
    Zip_code VARCHAR(12),
    Email VARCHAR(50) NOT NULL,
    Telephone VARCHAR(15) NOT NULL,
    Groupe_sanguin VARCHAR(10),
    Facebook VARCHAR(100),
    Instagram VARCHAR(100),
    Twitter VARCHAR(100),
    Tiktok VARCHAR(100),
    Linkedln VARCHAR(100)
);
SELECT* FROM Membres;

-- Table : Activites
CREATE TABLE Activites (
    ID_activite INT AUTO_INCREMENT PRIMARY KEY,
    Theme VARCHAR(50) NOT NULL,
    Date_activite DATE NOT NULL,
    Heure_debut TIME NOT NULL,
    Heure_fin TIME NOT NULL,
    Présentiel BOOLEAN DEFAULT TRUE NOT NULL,
    Lieu VARCHAR(25) NOT NULL,
    Ville_activite VARCHAR(25),
    Pays_activite VARCHAR(25),
    Type_activite ENUM("Reunion_statutaire", "Reunion_extraordianire", "Causerie/Conférence", 
    "Formation", "Communautaire", "Sorties") NOT NULL
);
SELECT* FROM Activites;

-- Table : Cotisations
CREATE TABLE Cotisations (
    ID_cotisation INT AUTO_INCREMENT PRIMARY KEY,
    ID_membre INT NOT NULL,
    Date_cotisation DATE NOT NULL,
    Montant DECIMAL(10,2) NOT NULL,
    Type_cotisation ENUM('Happy_gourdes', 'Amende', 'Frais_district', 'Dons') DEFAULT 'Dons',
    Mode_paiement VARCHAR(50),
    FOREIGN KEY (ID_membre) REFERENCES Membres(ID_membre)
        ON DELETE CASCADE
        ON UPDATE CASCADE
);
SELECT* FROM Cotisations;

-- Table : Presence (table d'association)
CREATE TABLE Presence (
    ID_presence INT AUTO_INCREMENT PRIMARY KEY,
    ID_membre INT NOT NULL,
    ID_activite INT NOT NULL,
    Statut ENUM('Présent', 'Absent', 'Excusé') DEFAULT 'Présent',
    FOREIGN KEY (ID_membre) REFERENCES Membres(ID_membre)
        ON DELETE CASCADE
        ON UPDATE CASCADE,
    FOREIGN KEY (ID_activite) REFERENCES Activites(ID_activite)
        ON DELETE CASCADE
        ON UPDATE CASCADE
);
SELECT* FROM Presence;