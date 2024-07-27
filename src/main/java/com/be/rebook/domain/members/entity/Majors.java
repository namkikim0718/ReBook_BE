package com.be.rebook.domain.members.entity;

import jakarta.persistence.*;
import lombok.Getter;

@Entity
@Getter
public class Majors {
    @Id
    @Column(name = "major_id")
    private String majorId;

    private String major;
}
