package com.be.rebook.domain.members.entity;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

@Entity
@Getter
public class Majors {
    @Id
    @Column(name = "major_id")
    private String majorId;

    private String major;
}
